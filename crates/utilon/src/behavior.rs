use std::cmp::Ordering;

use bevy::{
    ecs::schedule::{Chain, SystemConfigs},
    prelude::*,
    utils::NoOpHash,
};
use dashmap::DashMap;

use crate::{
    activity::{ActivityId, ActivityList, ActivitySeq},
    ScoringSet, TransitionSet, UtilonConfig,
};

pub struct BehaviorSettings {
    pub policy: Policy,
}

impl BehaviorSettings {
    pub fn maximum() -> Self {
        Self {
            policy: Policy::Maximum,
        }
    }

    pub fn threshold(threshold: f32) -> Self {
        Self {
            policy: Policy::Threshold(threshold),
        }
    }
}

pub trait UtilonAppExt {
    fn add_behavior<B: ActivitySeq>(&mut self, settings: BehaviorSettings);
}

impl UtilonAppExt for App {
    fn add_behavior<S: ActivitySeq>(&mut self, settings: BehaviorSettings) {
        let schedule = self.world().resource::<UtilonConfig>().schedule;
        S::init(self.world_mut());
        self.add_systems(
            schedule,
            (
                prepare_behaviors::<S>,
                make_scoring_system_set::<S>(&settings.policy, S::into_activity_list())
                    .in_set(ScoringSet),
                transition_action_states::<S>.in_set(TransitionSet),
            )
                .chain_ignore_deferred(),
        );
    }
}

#[derive(Component)]
pub struct Behavior<S: ActivitySeq> {
    scores: DashMap<ActivityId, (f32, u32), NoOpHash>,
    skip_remaining_scorers: bool,
    current_activity: ActivityState<ActivityId>,
    next_activity: Option<ActivityId>,
    now_millis: u32,
    _marker: std::marker::PhantomData<S>,
}

impl<S: ActivitySeq> Default for Behavior<S> {
    fn default() -> Self {
        Self {
            scores: DashMap::with_hasher(NoOpHash),
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
    pub fn score<const A: ActivityId>(&self, mut score: impl FnMut() -> f32) {
        if self.skip_remaining_scorers {
            return;
        }
        self.scores.insert(A, (score(), u32::MAX));
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
            .insert(A, (score(), self.now_millis + ttl_millis));
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
        behavior
            .scores
            .retain(|_, (_, cache)| *cache > behavior.now_millis);
    }
}

#[derive(Default, Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, Clone, Copy)]
pub enum Policy {
    Maximum,
    Threshold(f32),
}

fn pick_maximum<S: ActivitySeq>(mut behaviors: Query<&mut Behavior<S>>) {
    for mut behavior in behaviors.iter_mut() {
        if let Some(next_activity) = behavior
            .scores
            .iter()
            .max_by(|a, b| {
                a.value().0.partial_cmp(&b.value().0).unwrap_or_else(|| {
                    error!("NaN encountered");
                    Ordering::Less
                })
            })
            .map(|max| *max.key())
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
                .find(|r| r.value().0 >= threshold)
                .map(|r| *r.key())
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
                configs: vec![scorer_systems, pick_maximum::<S>.into_configs()],
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

    use crate::prelude::*;

    #[derive(Component)]
    struct InputA(f32);

    #[derive(Component, Default)]
    #[activity(always_09)]
    struct Activity09;

    fn always_09<const A: ActivityId, S: ActivitySeq>(query: Query<&Behavior<S>>) {
        for behavior in query.iter() {
            behavior.score::<A>(|| 0.9);
        }
    }

    #[derive(Component, Default)]
    #[activity(always_one)]
    struct Activity1;

    fn always_one<const A: ActivityId, S: ActivitySeq>(query: Query<&Behavior<S>>) {
        for behavior in query.iter() {
            behavior.score::<A>(|| 1.0);
        }
    }

    #[derive(Component, Default)]
    #[activity(always_nan)]
    struct ActivityNan;

    fn always_nan<const A: ActivityId, S: ActivitySeq>(query: Query<&Behavior<S>>) {
        for behavior in query.iter() {
            behavior.score::<A>(|| f32::NAN);
        }
    }

    #[derive(Component, Default)]
    #[activity(always_panic)]
    struct ActivityPanic;

    fn always_panic<const A: ActivityId, S: ActivitySeq>(query: Query<&Behavior<S>>) {
        for behavior in query.iter() {
            behavior.score::<A>(|| panic!("panic"));
        }
    }

    #[derive(Component, Default)]
    #[activity(identity)]
    struct ActivityIdentity;

    fn identity<const A: ActivityId, S: ActivitySeq>(query: Query<(&Behavior<S>, &InputA)>) {
        for (behavior, input) in query.iter() {
            behavior.score::<A>(|| input.0);
        }
    }

    #[test]
    fn test_maximum() {
        type S = (Activity09, Activity1, ActivityNan);

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
            (Order(4), Behavior::<S>::default(), InputA(f32::NAN)),
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
                ActivityState::Running(1),
            ]
        );
    }
}
