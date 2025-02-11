use std::any::TypeId;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::num::{NonZero, NonZeroU32};

use bevy::reflect::{FromType, ReflectFromPtr};
use bevy::utils::HashMap;
use bevy::{
    ecs::{
        component::ComponentId,
        intern::Interned,
        reflect::ReflectCommandExt,
        schedule::ScheduleLabel,
        system::{EntityCommand, EntityCommands},
    },
    prelude::*,
    reflect::{GetTypeRegistration, TypeRegistration},
};
use response::Response;

mod response;

pub mod prelude {
    pub use crate::{
        response::Response, Activity, Behavior, BehaviorBuilder, InsertBehavior, PrepareSet, Score,
        ScoreSet, TransitionSet, UtilonPlugin,
    };
}

#[repr(C)]
#[derive(Component, Reflect)]
#[reflect(Component, Default, Score)]
pub struct Score<A: Activity> {
    #[reflect(ignore)]
    scores: HashMap<ActivityHash, ScoreEntry>,
    response: Response,

    #[reflect(ignore)]
    _marker: std::marker::PhantomData<A>,
}

#[derive(Debug)]
struct ScoreEntry {
    activity: Box<dyn Reflect>,
    score: f32,
    ttl_ms: Option<NonZeroU32>,
}

impl<A: Activity + Debug> std::fmt::Debug for Score<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Score")
            .field("scores", &self.scores)
            .field("response_curve", &self.response)
            .finish()
    }
}

type ActivityHash = u64;

fn hash_activity<K: Hash + 'static>(key: &K) -> ActivityHash {
    use std::collections::hash_map::DefaultHasher;

    let mut hasher = DefaultHasher::new();
    TypeId::of::<K>().hash(&mut hasher);
    key.hash(&mut hasher);
    hasher.finish() & 0xFFFF_FFFF
}

#[derive(Clone, Debug)]
pub struct ReflectScore {
    max_score: for<'a> fn(value: &'a dyn Reflect) -> Option<(ActivityHash, &'a ScoreEntry)>,
    reset_activity: for<'a> fn(value: &'a mut dyn Reflect, delta_ms: u32),
}

impl<A: Activity> FromType<Score<A>> for ReflectScore {
    fn from_type() -> Self {
        ReflectScore {
            max_score: |value| {
                value.downcast_ref::<Score<A>>().and_then(|activity| {
                    activity
                        .scores
                        .iter()
                        .map(|(&hash, entry)| (hash, entry))
                        .max_by(|(_, a), (_, b)| a.score.partial_cmp(&b.score).unwrap())
                })
            },
            reset_activity: |value, delta_ms| {
                if let Some(score) = value.downcast_mut::<Score<A>>() {
                    score.reset(delta_ms);
                }
            },
        }
    }
}

impl<A: Activity> Default for Score<A> {
    fn default() -> Self {
        Self {
            scores: Default::default(),
            response: Response::Identity,
            _marker: std::marker::PhantomData::<A>,
        }
    }
}

impl<A: Activity<Key = A> + Hash> Score<A> {
    pub fn score(&mut self, activity: A, score: f32) {
        self.scores.insert(
            hash_activity(&activity),
            ScoreEntry {
                activity: Box::new(activity),
                score,
                ttl_ms: None,
            },
        );
    }
}

impl<A: Activity> Score<A> {
    pub fn score_cached(&mut self, ttl_ms: u32, key: A::Key, mut score: impl FnMut() -> (f32, A)) {
        if self.scores.contains_key(&hash_activity(&key)) {
            return;
        }
        let (score, activity) = score();
        let entry = ScoreEntry {
            activity: Box::new(activity),
            score,
            ttl_ms: NonZero::new(ttl_ms),
        };
        self.scores.insert(hash_activity(&key), entry);
    }

    /// Resets the TTL for each score entry by subtracting `delta_ms`.
    /// Removes entries whose TTL has expired.
    pub(crate) fn reset(&mut self, delta_ms: u32) {
        for entry in self.scores.values_mut() {
            if let Some(ttl) = entry.ttl_ms {
                entry.ttl_ms = NonZero::new(ttl.get().saturating_sub(delta_ms));
            }
        }
        self.scores
            .retain(|_, entry| entry.ttl_ms.map_or(false, |ttl| ttl.get() > 0));
    }
}

pub trait Activity: Default + Component + Reflect + GetTypeRegistration + TypePath {
    type Key: Hash;
}

#[derive(Debug, Component, Default)]
pub struct Behavior {
    activities: Vec<ActivityId>,
    current_activity: ActivityState<Box<dyn PartialReflect>>,
    next_activity: Option<(Box<dyn PartialReflect>, ActivityHash)>,
}

#[derive(Clone, Debug)]
struct ActivityId {
    activity_cid: ComponentId,
    score_cid: ComponentId,
    score_type_id: TypeId,
}

impl PartialEq for ActivityId {
    fn eq(&self, other: &Self) -> bool {
        self.activity_cid == other.activity_cid && self.score_cid == other.score_cid
    }
}

fn pick_maximum(mut world: &mut World) {
    let mut query = world.query_filtered::<EntityMut, With<Behavior>>();
    let type_registry = world.resource::<AppTypeRegistry>().clone();
    let type_reg = type_registry.read();

    for mut entity in query.iter_mut(&mut world) {
        let behavior = entity.get::<Behavior>().unwrap();
        let mut max = f32::NEG_INFINITY;
        let mut max_activity = None;
        let mut max_hash = 0;
        for activity in behavior.activities.iter() {
            let Ok(score_ptr) = entity.get_by_id(activity.score_cid) else {
                warn!("Activity {:?} has no Score", activity.score_cid);
                continue;
            };
            let score_type = type_reg.get(activity.score_type_id).unwrap();
            let reflect_score = score_type.data::<ReflectScore>().unwrap();
            let reflect_from_ptr = score_type.data::<ReflectFromPtr>().unwrap();
            let reflect = unsafe { reflect_from_ptr.as_reflect(score_ptr) };

            let score = (reflect_score.max_score)(reflect);
            if let Some((_hash, entry)) = score {
                if entry.score > max {
                    max_activity = Some(entry.activity.clone_value());
                    max = entry.score;
                    max_hash = _hash;
                }
            }
        }

        if let Some(max_activity) = max_activity {
            let mut behavior = entity.get_mut::<Behavior>().unwrap();
            if let ActivityState::Running(_current_activity, hash) = &behavior.current_activity {
                if hash != &max_hash {
                    behavior.next_activity = Some((max_activity, max_hash));
                }
            } else {
                behavior.next_activity = Some((max_activity, max_hash));
            }
        }
    }
}

pub struct UtilonPlugin {
    pub schedule: Interned<dyn ScheduleLabel>,
}

impl Default for UtilonPlugin {
    fn default() -> Self {
        Self {
            schedule: Update.intern(),
        }
    }
}

#[derive(SystemSet, Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct PrepareSet;

#[derive(SystemSet, Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct ScoreSet;

#[derive(SystemSet, Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct TransitionSet;

impl Plugin for UtilonPlugin {
    fn build(&self, app: &mut App) {
        app.configure_sets(self.schedule, (PrepareSet, ScoreSet, TransitionSet).chain());
        app.add_systems(self.schedule, prepare_behaviors.in_set(PrepareSet));
        app.add_systems(
            self.schedule,
            (pick_maximum, transition_activity_states)
                .chain_ignore_deferred()
                .in_set(TransitionSet),
        );
    }
}

pub struct BehaviorBuilder {
    activities: Vec<ActivityDescriptor>,
}

struct ActivityDescriptor {
    score_type_id: TypeId,
    activity_registration: TypeRegistration,
    score_registration: TypeRegistration,
    make_score: Box<dyn Fn() -> Box<dyn PartialReflect> + Send>,
}

impl BehaviorBuilder {
    pub fn new() -> Self {
        BehaviorBuilder {
            activities: Default::default(),
        }
    }

    pub fn with_activity<A: Activity>(mut self, response: Response) -> Self {
        self.activities.push(ActivityDescriptor {
            score_type_id: TypeId::of::<Score<A>>(),
            activity_registration: A::get_type_registration(),
            score_registration: Score::<A>::get_type_registration(),
            make_score: Box::new(move || {
                Box::new(Score::<A> {
                    response,
                    ..default()
                })
            }),
        });
        self
    }
}

pub trait InsertBehavior {
    fn insert_behavior(&mut self, behavior: BehaviorBuilder);
}

impl<'a> InsertBehavior for EntityCommands<'a> {
    fn insert_behavior(&mut self, behavior: BehaviorBuilder) {
        self.queue(BuildBehaviorCommand { behavior });
    }
}

pub struct BuildBehaviorCommand {
    behavior: BehaviorBuilder,
}

impl EntityCommand for BuildBehaviorCommand {
    fn apply(self, id: Entity, world: &mut World) {
        let mut behavior = Behavior::default();

        for desc in self.behavior.activities {
            let registry = world.resource_mut::<AppTypeRegistry>();
            registry
                .write()
                .add_registration(desc.activity_registration.clone());
            registry
                .write()
                .add_registration(desc.score_registration.clone());
            let activity_component = desc
                .activity_registration
                .data::<ReflectComponent>()
                .unwrap();
            let activity_cid = activity_component.register_component(world);
            let score_component = desc.score_registration.data::<ReflectComponent>().unwrap();
            let score_cid = score_component.register_component(world);
            world
                .commands()
                .entity(id)
                .insert_reflect(desc.make_score.as_ref()());

            behavior.activities.push(ActivityId {
                activity_cid: activity_cid,
                score_cid: score_cid,
                score_type_id: desc.score_type_id,
            });
        }

        world.commands().entity(id).insert(behavior);
    }
}

#[derive(Default, Debug, PartialEq, Eq, Clone, Reflect)]
pub enum ActivityState<A> {
    #[default]
    None,
    Running(A, ActivityHash),
    Success(A, ActivityHash),
    Failure(A, ActivityHash),
}

#[derive(Event)]
pub struct OnCanceled;

#[derive(Event)]
pub struct OnFailure;

#[derive(Event)]
pub struct OnSuccess;

fn transition_activity_states(
    mut commands: Commands,
    mut behaviors: Query<(Entity, &mut Behavior)>,
) {
    for (entity, mut behavior) in behaviors.iter_mut() {
        if let Some((next_activity, next_hash)) = &behavior.next_activity {
            if let ActivityState::Running(current, _) = &behavior.current_activity {
                let removed = current
                    .get_represented_type_info()
                    .map(|t| t.type_path())
                    .unwrap();
                commands.trigger_targets(OnCanceled, entity);
                commands.entity(entity).remove_reflect(removed);

                debug!("Removed activity {:?} from entity {:?}", removed, entity);
            }
            commands
                .entity(entity)
                .insert_reflect(next_activity.clone_value());
            debug!(
                "Inserted activity {:?} into entity {:?}",
                next_activity, entity
            );
            behavior.current_activity =
                ActivityState::Running(next_activity.clone_value(), *next_hash);
            behavior.next_activity = None;
        } else {
            match &behavior.current_activity {
                ActivityState::Success(current, _) => {
                    commands
                        .entity(entity)
                        .remove_reflect(current.reflect_type_path().to_owned());
                    behavior.current_activity = ActivityState::None;
                    commands.trigger_targets(OnSuccess, entity);
                }
                ActivityState::Failure(current, _) => {
                    commands
                        .entity(entity)
                        .remove_reflect(current.reflect_type_path().to_owned());
                    behavior.current_activity = ActivityState::None;
                    commands.trigger_targets(OnFailure, entity);
                }
                ActivityState::Running(_, _) | ActivityState::None => {}
            }
        }
    }
}

fn advance_scores(mut world: &mut World, delta_ms: u32) {
    let mut behaviors = world.query_filtered::<EntityMut, With<Behavior>>();
    let type_registry = world.resource::<AppTypeRegistry>().clone();
    let type_reg = type_registry.read();

    for mut entity in behaviors.iter_mut(&mut world) {
        let activities = entity
            .get::<Behavior>()
            .unwrap()
            .activities
            .clone()
            .into_iter()
            .collect::<Vec<_>>();
        for activity in activities.iter() {
            let activity_scores = entity
                .get_mut_by_id(activity.score_cid)
                .unwrap()
                .into_inner();
            let score_type = type_reg.get(activity.score_type_id).unwrap();
            let reflect_score = score_type.data::<ReflectScore>().unwrap();
            let reflect_from_ptr = score_type.data::<ReflectFromPtr>().unwrap();
            let reflect = unsafe { reflect_from_ptr.as_reflect_mut(activity_scores) };
            (reflect_score.reset_activity)(reflect, delta_ms);
        }
    }
}

fn prepare_behaviors(mut world: &mut World) {
    let time = world.resource::<Time>();
    let delta_ms = (time.delta_secs_f64() * 1000.0) as u32;
    advance_scores(&mut world, delta_ms);
}

#[cfg(test)]
mod tests {

    use super::*;

    #[derive(Component, Reflect, Default, Hash, Debug)]
    #[reflect(Component)] // todo: remove for bevy 0.15
    struct Idle;

    impl Activity for Idle {
        type Key = Idle;
    }

    fn score_idle(mut query: Query<&mut Score<Idle>>) {
        for mut scorer in query.iter_mut() {
            scorer.score(Idle, 0.5);
        }
    }

    #[derive(Component, Reflect, Default, Hash, Debug)]
    #[reflect(Component)] // todo: remove for bevy 0.15
    struct Pursue;

    impl Activity for Pursue {
        type Key = Pursue;
    }

    fn score_pursue(mut query: Query<&mut Score<Pursue>>) {
        for mut scorer in query.iter_mut() {
            scorer.score(Pursue, 1.0);
        }
    }

    #[test]
    fn test_basic_state_transition() {
        let mut app = App::new();
        app.add_plugins((MinimalPlugins, UtilonPlugin::default()));
        app.add_systems(Update, (score_idle, score_pursue).in_set(ScoreSet));
        app.world_mut().commands().spawn_empty().insert_behavior(
            BehaviorBuilder::new()
                .with_activity::<Idle>(Response::Identity)
                .with_activity::<Pursue>(Response::Identity),
        );

        app.update();

        let w = app.world_mut();

        // This shoud not panic
        w.query::<(&Behavior, &Score<Idle>, &Score<Pursue>, &Pursue)>()
            .single(&w);

        assert_eq!(
            w.query::<&Behavior>().single(&w).next_activity.is_none(),
            true
        );
    }

    #[test]
    fn test_activities_distinct_hashes() {
        assert_ne!(hash_activity(&Idle), hash_activity(&Pursue));

        #[derive(Component, Reflect, Default, Hash, Debug)]
        #[reflect(Component)] // todo: remove for bevy 0.15
        struct A {
            a: u32,
        }

        assert_ne!(hash_activity(&A { a: 0 }), hash_activity(&A { a: 1 }));
    }

    #[test]
    fn test_score_cached() {
        #[derive(Component, Reflect, Default, Hash, Debug)]
        #[reflect(Component)] // todo: remove for bevy 0.15
        struct PonderKey;

        #[derive(Component, Reflect, Default, Debug)]
        #[reflect(Component)] // todo: remove for bevy 0.15
        struct Ponder {
            result: f32,
        }

        impl Activity for Ponder {
            type Key = PonderKey;
        }

        fn score_ponder(mut query: Query<&mut Score<Ponder>>, mut counter: Local<u32>) {
            for mut scorer in query.iter_mut() {
                scorer.score_cached(10000, PonderKey, || {
                    if *counter > 0 {
                        panic!("cached score was recalculated");
                    }
                    (
                        (0.9 / (*counter as f32 + 1.0)),
                        Ponder {
                            result: *counter as f32,
                        },
                    )
                });
                *counter += 1;
            }
        }

        let mut app = App::new();
        app.add_plugins((MinimalPlugins, UtilonPlugin::default()));
        app.add_systems(Update, (score_idle, score_ponder).in_set(ScoreSet));
        app.world_mut().commands().spawn_empty().insert_behavior(
            BehaviorBuilder::new()
                .with_activity::<Idle>(Response::Identity)
                .with_activity::<Ponder>(Response::Identity),
        );

        app.update();
        app.update();

        let w = app.world_mut();
        let (ponder, score) = w.query::<(&Ponder, &Score<Ponder>)>().single(&w);
        assert_eq!(ponder.result, 0.0);
        assert_eq!(score.scores.len(), 1);
    }

    #[test]
    fn test_ttl_expiry() {
        #[derive(Component, Reflect, Default, Hash, Debug)]
        #[reflect(Component)] // todo: remove for bevy 0.15
        struct PonderKey;

        #[derive(Component, Reflect, Default, Debug)]
        #[reflect(Component)] // todo: remove for bevy 0.15
        struct Ponder {
            result: f32,
        }

        impl Activity for Ponder {
            type Key = PonderKey;
        }

        fn score_ponder(mut query: Query<&mut Score<Ponder>>, mut counter: Local<u32>) {
            for mut scorer in query.iter_mut() {
                scorer.score_cached(10000, PonderKey, || {
                    let score = 0.9 / (*counter as f32 + 1.0);
                    (
                        score,
                        Ponder {
                            result: *counter as f32,
                        },
                    )
                });
                *counter += 1;
            }
        }

        let mut app = App::new();
        app.add_plugins((MinimalPlugins, UtilonPlugin::default()));
        app.add_systems(Update, (score_idle, score_ponder).in_set(ScoreSet));
        app.world_mut().commands().spawn_empty().insert_behavior(
            BehaviorBuilder::new()
                .with_activity::<Idle>(Response::Identity)
                .with_activity::<Ponder>(Response::Identity),
        );

        app.update();
        advance_scores(&mut app.world_mut(), 10001);
        app.update();

        let w = app.world_mut();
        w.query::<(&Idle,)>().single(&w);
    }

    #[test]
    fn complex_keys() {
        #[derive(Hash)]
        struct AttackKey {
            player_id: u32,
        }

        #[derive(Component, Reflect, Default, Debug)]
        #[reflect(Component)] // todo: remove for bevy 0.15
        struct Attack {
            player_id: u32,
            angle: f32,
        }

        impl Activity for Attack {
            type Key = AttackKey;
        }

        fn score_attack(mut query: Query<&mut Score<Attack>>) {
            for mut scorer in query.iter_mut() {
                scorer.score_cached(0, AttackKey { player_id: 0 }, || {
                    (
                        0.2,
                        Attack {
                            player_id: 0,
                            angle: 0.0,
                        },
                    )
                });
                scorer.score_cached(0, AttackKey { player_id: 1 }, || {
                    (
                        0.8,
                        Attack {
                            player_id: 1,
                            angle: 1.0,
                        },
                    )
                });
            }
        }

        let mut app = App::new();
        app.add_plugins((MinimalPlugins, UtilonPlugin::default()));
        app.add_systems(Update, (score_idle, score_attack).in_set(ScoreSet));
        app.world_mut().commands().spawn_empty().insert_behavior(
            BehaviorBuilder::new()
                .with_activity::<Idle>(Response::Identity)
                .with_activity::<Attack>(Response::Identity),
        );

        app.update();

        let w = app.world_mut();
        let (attack, score) = w.query::<(&Attack, &Score<Attack>)>().single(&w);
        assert_eq!(attack.angle, 1.0);
        assert_eq!(attack.player_id, 1);
        assert_eq!(score.scores.len(), 2);
    }
}
