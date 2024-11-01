use std::{cmp::Ordering, sync::atomic::AtomicU32};

use atomic_float::AtomicF32;
use bevy::{
    ecs::schedule::{Chain, SystemConfigs},
    prelude::*,
    reflect::{utility::GenericTypePathCell, TypePath},
};

use crate::{
    activity::{ActivityId, ActivityList, ActivitySeq},
    response::ResponseCurve,
    UtilonConfig,
};

#[derive(Resource, Reflect)]
pub struct BehaviorSettings<S: ActivitySeq> {
    pub policy: Policy,
    pub response_curves: Vec<ResponseCurve>,
    #[reflect(ignore)]
    _marker: std::marker::PhantomData<S>,
}

impl<S: ActivitySeq> BehaviorSettings<S> {
    pub fn maximum() -> Self {
        Self {
            policy: Policy::Maximum,
            response_curves: Default::default(),
            _marker: Default::default(),
        }
    }

    pub fn threshold(threshold: f32) -> Self {
        Self {
            policy: Policy::Threshold(threshold),
            response_curves: Default::default(),
            _marker: Default::default(),
        }
    }

    pub fn with_responses(mut self, responses: impl IntoIterator<Item = ResponseCurve>) -> Self {
        self.response_curves = responses.into_iter().collect();
        self
    }
}

pub trait UtilonAppExt {
    fn add_behavior<S: ActivitySeq>(&mut self, settings: BehaviorSettings<S>);
}

impl UtilonAppExt for App {
    fn add_behavior<S: ActivitySeq>(&mut self, settings: BehaviorSettings<S>) {
        let schedule = self.world().resource::<UtilonConfig>().schedule;
        S::init(self.world_mut());
        self.register_type::<Behavior<S>>();
        self.add_systems(
            schedule,
            (
                prepare_behaviors::<S>,
                make_scoring_system_set::<S>(&settings.policy, S::into_activity_list()),
                transition_action_states::<S>,
            )
                .chain_ignore_deferred(),
        );
        self.insert_resource(settings);
    }
}

#[derive(Component, Reflect)]
#[reflect(type_path = false)]
pub struct Behavior<S: ActivitySeq> {
    #[reflect(ignore)]
    scores: Vec<Score>,
    skip_remaining_scorers: bool,
    current_activity: ActivityState<ActivityId>,
    next_activity: Option<ActivityId>,
    now_millis: u32,
    #[reflect(ignore)]
    _marker: std::marker::PhantomData<S>,
}

#[derive(Debug)]
struct Score {
    score: AtomicF32,
    ttl: AtomicU32,
}

impl Score {
    fn get_score(&self) -> f32 {
        self.score.load(core::sync::atomic::Ordering::Relaxed)
    }

    fn store(&self, value: (f32, u32)) {
        self.score
            .store(value.0, core::sync::atomic::Ordering::Relaxed);
        self.ttl
            .store(value.1, core::sync::atomic::Ordering::Relaxed);
    }

    fn store_score(&self, value: f32) {
        self.score
            .store(value, core::sync::atomic::Ordering::Relaxed);
    }

    fn is_outdated(&self, now_millis: u32) -> bool {
        self.ttl.load(core::sync::atomic::Ordering::Relaxed) < now_millis
    }
}

impl Default for Score {
    fn default() -> Self {
        Self {
            score: AtomicF32::new(0.0),
            ttl: AtomicU32::new(0),
        }
    }
}

impl Clone for Score {
    fn clone(&self) -> Self {
        Self {
            score: AtomicF32::new(self.score.load(core::sync::atomic::Ordering::Relaxed)),
            ttl: AtomicU32::new(self.ttl.load(core::sync::atomic::Ordering::Relaxed)),
        }
    }
}

impl<S: ActivitySeq> TypePath for Behavior<S> {
    fn type_path() -> &'static str {
        static CELL: GenericTypePathCell = GenericTypePathCell::new();
        CELL.get_or_insert::<Self, _>(|| format!("utilon::Behavior<{}>", S::type_path()))
    }

    fn short_type_path() -> &'static str {
        static CELL: GenericTypePathCell = GenericTypePathCell::new();
        CELL.get_or_insert::<Self, _>(|| format!("utilon::Behavior<{}>", S::short_type_path()))
    }
}

impl<S: ActivitySeq> Default for Behavior<S> {
    fn default() -> Self {
        Self {
            scores: Default::default(),
            skip_remaining_scorers: false,
            current_activity: Default::default(),
            next_activity: Default::default(),
            now_millis: 0,
            _marker: Default::default(),
        }
    }
}

impl<S: ActivitySeq> Behavior<S> {
    #[inline]
    pub fn score<const A: ActivityId>(&self, score: impl FnMut() -> f32) {
        if self.skip_remaining_scorers {
            return;
        }
        self.score_with_ttl::<A>(score, 0);
    }

    #[inline]
    pub fn score_with_ttl<const A: ActivityId>(
        &self,
        mut score: impl FnMut() -> f32,
        ttl_millis: u32,
    ) {
        if self.skip_remaining_scorers {
            return;
        }
        self.scores
            .get(A as usize)
            .unwrap()
            .store((score(), self.now_millis + ttl_millis));
    }

    pub fn succeed(&mut self) {
        if let ActivityState::Running(current) = self.current_activity {
            self.current_activity = ActivityState::Success(current);
        }
    }

    pub fn fail(&mut self) {
        if let ActivityState::Running(current) = self.current_activity {
            self.current_activity = ActivityState::Failure(current);
        }
    }
}

fn prepare_behaviors<S: ActivitySeq>(mut behaviors: Query<&mut Behavior<S>>, time: Res<Time>) {
    let now_millis = (time.elapsed_seconds() * 1000.0).floor() as u32;

    for mut behavior in behaviors.iter_mut() {
        behavior.skip_remaining_scorers = false;
        behavior.next_activity = None;
        behavior.now_millis = now_millis;
        behavior.scores.resize(S::count(), Score::default());
        behavior.scores.iter_mut().for_each(|s| {
            *s = if s.is_outdated(now_millis) {
                Default::default()
            } else {
                s.clone()
            };
        });
    }
}

fn apply_responses<S: ActivitySeq>(
    mut behaviors: Query<&mut Behavior<S>>,
    settings: Res<BehaviorSettings<S>>,
) {
    for mut behavior in behaviors.iter_mut() {
        for (i, score) in behavior.scores.iter_mut().enumerate() {
            if let Some(curve) = settings.response_curves.get(i) {
                score.store_score(curve.eval(score.get_score()));
            }
        }
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

fn transition_action_states<S: ActivitySeq>(
    mut commands: Commands,
    mut behaviors: Query<(Entity, &mut Behavior<S>)>,
) {
    for (entity, mut behavior) in behaviors.iter_mut() {
        if let Some(next_activity) = behavior.next_activity {
            if let ActivityState::Running(current) = behavior.current_activity {
                commands.trigger_targets(OnCanceled, entity);
                S::exit(current, &mut commands.entity(entity));
            }
            S::enter(next_activity, &mut commands.entity(entity));
            behavior.current_activity = ActivityState::Running(next_activity);
            behavior.next_activity = None;
        } else {
            match behavior.current_activity {
                ActivityState::Success(current) => {
                    S::exit(current, &mut commands.entity(entity));
                    behavior.current_activity = ActivityState::None;
                    commands.trigger_targets(OnSuccess, entity);
                }
                ActivityState::Failure(current) => {
                    S::exit(current, &mut commands.entity(entity));
                    behavior.current_activity = ActivityState::None;
                    commands.trigger_targets(OnFailure, entity);
                }
                ActivityState::Running(_) | ActivityState::None => {}
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Reflect)]
pub enum Policy {
    Maximum,
    Threshold(f32),
}

fn pick_maximum<S: ActivitySeq>(mut behaviors: Query<&mut Behavior<S>>) {
    for mut behavior in behaviors.iter_mut() {
        if let Some(next_activity) = behavior
            .scores
            .iter()
            .enumerate()
            .max_by(|(_, a), (_, b)| {
                a.get_score()
                    .partial_cmp(&b.get_score())
                    .unwrap_or_else(|| {
                        error!("NaN encountered");
                        Ordering::Less
                    })
            })
            .map(|(i, _)| i as u8)
        {
            behavior.next_activity = Some(next_activity);
        } else {
            warn!("No activity produced a score");
        };
    }
}

fn pick_first_to_threshold<S: ActivitySeq>(threshold: f32) -> impl Fn(Query<&mut Behavior<S>>) {
    move |mut behaviors: Query<&mut Behavior<S>>| {
        behaviors.iter_mut().for_each(|mut behavior| {
            let Some(next_activity) = behavior
                .scores
                .iter()
                .enumerate()
                .find(|(_, r)| r.get_score() >= threshold)
                .map(|(i, _)| i as u8)
            else {
                return;
            };
            behavior.next_activity = Some(next_activity);
            behavior.skip_remaining_scorers = true;
        })
    }
}

pub fn make_scoring_system_set<S: ActivitySeq>(
    policy: &Policy,
    activities: ActivityList,
) -> SystemConfigs {
    let configs = activities.0.into_iter();

    match policy {
        Policy::Threshold(threshold) => SystemConfigs::Configs {
            configs: configs
                .zip(std::iter::repeat_with(|| {
                    pick_first_to_threshold::<S>(*threshold).into_configs()
                }))
                .map(|(score, pick)| SystemConfigs::Configs {
                    // todo: support response curves
                    configs: vec![score, pick],
                    collective_conditions: vec![],
                    chained: Chain::YesIgnoreDeferred,
                })
                .collect(),
            collective_conditions: vec![],
            chained: Chain::YesIgnoreDeferred,
        },
        Policy::Maximum => {
            let scorer_systems = SystemConfigs::Configs {
                configs: configs.collect(),
                collective_conditions: vec![],
                chained: Chain::No,
            };
            let picker_systems = SystemConfigs::Configs {
                configs: vec![
                    scorer_systems,
                    apply_responses::<S>.into_configs(),
                    pick_maximum::<S>.into_configs(),
                ],
                collective_conditions: vec![],
                chained: Chain::YesIgnoreDeferred,
            };
            picker_systems
        }
    }
}

#[cfg(test)]
mod tests {

    use bevy::prelude::*;

    use crate::{prelude::*, scorers::always_one};

    #[derive(Component)]
    struct InputA(f32);

    #[derive(Component, Default, Reflect)]
    #[activity(always_09)]
    struct Activity09;

    fn always_09<const A: ActivityId, S: ActivitySeq>(query: Query<&Behavior<S>>) {
        for behavior in query.iter() {
            behavior.score::<A>(|| 0.9);
        }
    }

    #[derive(Component, Default, Reflect)]
    #[activity(always_one)]
    struct Activity1;

    #[derive(Component, Default, Reflect)]
    #[activity(always_nan)]
    struct ActivityNan;

    fn always_nan<const A: ActivityId, S: ActivitySeq>(query: Query<&Behavior<S>>) {
        for behavior in query.iter() {
            behavior.score::<A>(|| f32::NAN);
        }
    }

    #[derive(Component, Default, Reflect)]
    #[activity(always_panic)]
    struct ActivityPanic;

    fn always_panic<const A: ActivityId, S: ActivitySeq>(query: Query<&Behavior<S>>) {
        for behavior in query.iter() {
            behavior.score::<A>(|| panic!("panic"));
        }
    }

    #[derive(Component, Default, Reflect)]
    #[activity(identity)]
    struct ActivityIdentity;

    fn identity<const A: ActivityId, S: ActivitySeq>(query: Query<(&Behavior<S>, &InputA)>) {
        for (behavior, input) in query.iter() {
            behavior.score::<A>(|| input.0);
        }
    }

    #[test]
    fn test_maximum() {
        type S = (Activity09, Activity1);

        let mut app = App::new();
        app.add_plugins((MinimalPlugins, UtilonPlugin::default()));
        app.add_behavior::<S>(BehaviorSettings::maximum());
        app.world_mut().spawn((Behavior::<S>::default(),));

        let b = app.world_mut().query::<&Behavior<S>>().single(&app.world());
        assert_eq!(b.next_activity, None);

        app.update();
        let b = app.world_mut().query::<&Behavior<S>>().single(&app.world());
        assert_eq!(b.next_activity, None);
        assert_eq!(b.current_activity, ActivityState::Running(1));
    }

    #[test]
    fn test_threshold_selects_first() {
        type S = (Activity09, Activity1, ActivityNan);

        let mut app = App::new();
        app.add_plugins((MinimalPlugins, UtilonPlugin::default()));
        app.add_behavior::<S>(BehaviorSettings::threshold(0.9));
        app.world_mut().spawn((Behavior::<S>::default(),));

        let b = app.world_mut().query::<&Behavior<S>>().single(&app.world());
        assert_eq!(b.next_activity, None);

        app.update();
        let b = app.world_mut().query::<&Behavior<S>>().single(&app.world());
        assert_eq!(b.next_activity, None);
        assert_eq!(b.current_activity, ActivityState::Running(0));
    }

    #[test]
    fn test_threshold_lazy_evaluation() {
        type S = (Activity09, ActivityPanic);

        let mut app = App::new();
        app.add_plugins((MinimalPlugins, UtilonPlugin::default()));
        app.add_behavior::<S>(BehaviorSettings::threshold(0.9));
        app.world_mut().spawn((Behavior::<S>::default(),));

        let b = app.world_mut().query::<&Behavior<S>>().single(&app.world());
        assert_eq!(b.next_activity, None);

        app.update();
        let b = app.world_mut().query::<&Behavior<S>>().single(&app.world());
        assert_eq!(b.next_activity, None);
        assert_eq!(b.current_activity, ActivityState::Running(0));
    }

    #[test]
    fn test_all_below_threshold() {
        type S = (Activity09, Activity1);
        let mut app = App::new();
        app.add_plugins((MinimalPlugins, UtilonPlugin::default()));
        app.add_behavior::<S>(BehaviorSettings::threshold(2.0));
        app.world_mut().spawn((Behavior::<S>::default(),));

        app.update();
        let b = app.world_mut().query::<&Behavior<S>>().single(&app.world());

        assert_eq!(b.current_activity, ActivityState::None);
    }

    #[test]
    fn test_many_entities() {
        type S = (Activity09, ActivityIdentity);

        #[derive(Component, PartialEq, Eq, Hash, Debug, Copy, Clone, Ord, PartialOrd)]
        struct Order(u32);

        let mut app = App::new();
        app.add_plugins((MinimalPlugins, UtilonPlugin::default()));
        app.add_behavior::<S>(BehaviorSettings::maximum());
        app.world_mut().spawn_batch([
            (Order(0), Behavior::<S>::default(), InputA(0.8)),
            (Order(1), Behavior::<S>::default(), InputA(0.1)),
            (Order(2), Behavior::<S>::default(), InputA(0.0)),
            (Order(3), Behavior::<S>::default(), InputA(1.0)),
        ]);

        app.update();
        let b = app
            .world_mut()
            .query::<(&Order, &Behavior<S>)>()
            .iter(&app.world())
            .sort_by::<(&Order, &Behavior<S>)>(|a, b| a.0.cmp(b.0))
            .map(|(_, b)| b.current_activity.clone())
            .collect::<Vec<_>>();
        assert_eq!(
            b,
            vec![
                ActivityState::Running(0),
                ActivityState::Running(0),
                ActivityState::Running(0),
                ActivityState::Running(1),
            ]
        );
    }

    #[test]
    fn test_responses() {
        type S = (Activity09, Activity1);

        let mut app = App::new();
        app.add_plugins((MinimalPlugins, UtilonPlugin::default()));
        app.add_behavior::<S>(BehaviorSettings::maximum().with_responses([
            ResponseCurve::InverseLerp { min: 0.0, max: 0.1 },
            ResponseCurve::Sigmoid {
                steepness: 1.0,
                center: 2.0,
            },
        ]));
        app.world_mut().spawn((Behavior::<S>::default(),));

        app.update();
        let b = app.world_mut().query::<&Behavior<S>>().single(&app.world());
        assert_eq!(b.current_activity, ActivityState::Running(0));
    }
}
