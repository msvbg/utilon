use std::{any::TypeId, sync::Arc};

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

#[repr(C)]
#[derive(Component, Reflect)]
#[reflect(Component, Default)]
pub struct Score<A: Activity> {
    score: f32,
    ttl_millis: u32,
    response: Response,

    #[reflect(ignore)]
    _marker: std::marker::PhantomData<A>,
}

impl<A: Activity> std::fmt::Debug for Score<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Score")
            .field("score", &self.score)
            .field("ttl_millis", &self.ttl_millis)
            .field("response_curve", &self.response)
            .field("_marker", &self._marker)
            .finish()
    }
}

#[derive(Reflect)]
#[repr(C)]
struct UntypedScore {
    score: f32,
    ttl_millis: u32,
    response_curve: Response,
}

impl<A: Activity> Default for Score<A> {
    fn default() -> Self {
        Self {
            score: Default::default(),
            ttl_millis: 0,
            response: Response::Identity,
            _marker: Default::default(),
        }
    }
}

impl<A: Activity> Score<A> {
    pub fn score(&mut self, mut score: impl FnMut() -> f32) {
        self.score = score();
    }
}

pub trait Activity: Default + Component + Reflect + GetTypeRegistration + TypePath {}

impl<A> Activity for A where A: Default + Component + Reflect + GetTypeRegistration + TypePath {}

#[derive(Debug, Component, Default)]
struct Behavior {
    activities: Vec<ActivityId>,
    current_activity: ActivityState<ActivityId>,
    next_activity: Option<ActivityId>,
}

#[derive(Clone, Debug)]
struct ActivityId {
    activity_default: Arc<dyn Reflect>,
    activity_cid: ComponentId,
    score_cid: ComponentId,
}

impl PartialEq for ActivityId {
    fn eq(&self, other: &Self) -> bool {
        self.activity_cid == other.activity_cid && self.score_cid == other.score_cid
    }
}

impl ActivityId {
    fn enter(&self, ec: &mut EntityCommands) {
        ec.insert_reflect(self.activity_default.clone_value());
    }

    fn exit(&self, ec: &mut EntityCommands) {
        ec.remove_by_id(self.activity_cid);
    }
}

fn pick_maximum(mut query: Query<EntityMut, With<Behavior>>) {
    for mut entity in query.iter_mut() {
        let behavior = entity.get::<Behavior>().unwrap();
        let mut max = f32::NEG_INFINITY;
        let mut max_activity = None;
        for activity in behavior.activities.iter() {
            let Some(score_ptr) = entity.get_by_id(activity.score_cid) else {
                warn!("Activity {:?} has no Score", activity.score_cid);
                continue;
            };
            // SAFETY: Score<_> has the same layout as UntypedScore
            let untyped_score = unsafe { score_ptr.deref::<UntypedScore>() };
            let score = untyped_score.response_curve.eval(untyped_score.score);
            if score > max {
                max_activity = Some(activity.clone());
                max = score;
            }
        }

        let mut behavior = entity.get_mut::<Behavior>().unwrap();
        behavior.next_activity = max_activity;
    }
}

pub struct UtilonPlugin {
    schedule: Interned<dyn ScheduleLabel>,
}

impl Default for UtilonPlugin {
    fn default() -> Self {
        Self {
            schedule: PostUpdate.intern(),
        }
    }
}

impl Plugin for UtilonPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            self.schedule,
            (pick_maximum, transition_activity_states).chain_ignore_deferred(),
        );
    }
}

pub struct BehaviorBuilder {
    activities: Vec<ActivityDescriptor>,
}

struct ActivityDescriptor {
    activity_type_id: TypeId,
    score_type_id: TypeId,
    activity_registration: TypeRegistration,
    score_registration: TypeRegistration,
    make_activity: Box<dyn Fn() -> Arc<dyn Reflect> + Send>,
    make_score: Box<dyn Fn() -> Box<dyn Reflect> + Send>,
}

impl BehaviorBuilder {
    pub fn new() -> Self {
        BehaviorBuilder {
            activities: Default::default(),
        }
    }

    pub fn with_activity<A: Activity>(mut self, response: Response) -> Self {
        self.activities.push(ActivityDescriptor {
            activity_type_id: TypeId::of::<A>(),
            score_type_id: TypeId::of::<Score<A>>(),
            activity_registration: A::get_type_registration(),
            score_registration: Score::<A>::get_type_registration(),
            make_activity: Box::new(move || Arc::new(A::default())),
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

#[derive(Default)]
enum SelectionPolicy {
    #[default]
    Maximum,
}

pub trait InsertBehavior {
    fn insert_behavior(&mut self, behavior: BehaviorBuilder);
}

impl<'a> InsertBehavior for EntityCommands<'a> {
    fn insert_behavior(&mut self, behavior: BehaviorBuilder) {
        self.add(BuildBehaviorCommand { behavior });
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
            // todo: uncomment when upgrading to bevy 0.15, and remove the
            // flush_commands. also remove the cursed activity insert/remove below.
            // let reflect_component = registration.data::<ReflectComponent>().unwrap();
            // reflect_component.register_component(world);
            world
                .commands()
                .entity(id)
                .insert_reflect(desc.make_activity.as_ref()().clone_value());
            world
                .commands()
                .entity(id)
                .insert_reflect(desc.make_score.as_ref()());

            world.flush_commands();

            let activity_cid = world.components().get_id(desc.activity_type_id).unwrap();
            world.commands().entity(id).remove_by_id(activity_cid);

            behavior.activities.push(ActivityId {
                activity_default: desc.make_activity.as_ref()(),
                activity_cid: activity_cid,
                score_cid: world.components().get_id(desc.score_type_id).unwrap(),
            });
        }

        world.commands().entity(id).insert(behavior);
    }
}

#[derive(Default, Debug, PartialEq, Eq, Clone, Reflect)]
pub enum ActivityState<A> {
    #[default]
    None,
    Running(A),
    Success(A),
    Failure(A),
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
        if let Some(next_activity) = &behavior.next_activity {
            if let ActivityState::Running(current) = &behavior.current_activity {
                commands.trigger_targets(OnCanceled, entity);
                current.exit(&mut commands.entity(entity));
            }
            next_activity.enter(&mut commands.entity(entity));
            behavior.current_activity = ActivityState::Running(next_activity.clone());
            behavior.next_activity = None;
        } else {
            match &behavior.current_activity {
                ActivityState::Success(current) => {
                    current.exit(&mut commands.entity(entity));
                    behavior.current_activity = ActivityState::None;
                    commands.trigger_targets(OnSuccess, entity);
                }
                ActivityState::Failure(current) => {
                    current.exit(&mut commands.entity(entity));
                    behavior.current_activity = ActivityState::None;
                    commands.trigger_targets(OnFailure, entity);
                }
                ActivityState::Running(_) | ActivityState::None => {}
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Component, Reflect, Default)]
    #[reflect(Component)] // todo: remove for bevy 0.15
    struct Idle;

    fn score_idle(mut query: Query<&mut Score<Idle>>) {
        for mut scorer in query.iter_mut() {
            scorer.score(|| 0.0);
        }
    }

    #[derive(Component, Reflect, Default)]
    #[reflect(Component)] // todo: remove for bevy 0.15
    struct Pursue;

    fn score_pursue(mut query: Query<&mut Score<Pursue>>) {
        for mut scorer in query.iter_mut() {
            scorer.score(|| 1.0);
        }
    }

    #[test]
    fn test_untyped_cast() {
        assert_eq!(size_of::<UntypedScore>(), size_of::<Score<Idle>>());
        assert_eq!(align_of::<UntypedScore>(), align_of::<Score<Idle>>());

        let score = Score {
            score: 42.0,
            ttl_millis: 123,
            response: Response::Sigmoid {
                steepness: 2.0,
                center: 3.0,
            },
            _marker: std::marker::PhantomData::<Idle>,
        };

        let untyped = unsafe {
            (&score as *const _ as *const UntypedScore)
                .as_ref()
                .unwrap()
        };
        assert_eq!(untyped.score, score.score);
        assert_eq!(untyped.ttl_millis, score.ttl_millis);
        assert_eq!(untyped.response_curve, score.response);
    }

    #[test]
    fn basic_state_transition() {
        let mut app = App::new();
        app.add_plugins((MinimalPlugins, UtilonPlugin::default()));
        app.add_systems(Update, (score_idle, score_pursue));
        app.world_mut().commands().spawn_empty().insert_behavior(
            BehaviorBuilder::new()
                .with_activity::<Idle>(Response::Identity)
                .with_activity::<Pursue>(Response::Identity),
        );

        app.update();
        app.world_mut().flush_commands();
        app.update();

        let w = app.world_mut();

        // This shoud not panic
        w.query::<(&Behavior, &Score<Idle>, &Score<Pursue>, &Pursue)>()
            .single(&w);

        assert_eq!(w.query::<&Behavior>().single(&w).next_activity, None);
        assert_eq!(
            w.query::<&Behavior>().single(&w).current_activity.clone(),
            ActivityState::Running(ActivityId {
                activity_default: Arc::new(Pursue::default()),
                activity_cid: w.init_component::<Pursue>(),
                score_cid: w.init_component::<Score<Pursue>>()
            })
        );
    }
}
