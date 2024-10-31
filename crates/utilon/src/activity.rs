use bevy::{
    ecs::{schedule::SystemConfigs, system::EntityCommands},
    prelude::*,
    utils::all_tuples,
};
use utilon_macros::impl_activity_seq;

pub trait Activity: TypePath + Component + Default {
    fn init(world: &mut World);
    fn system<const A: ActivityId, S: ActivitySeq>() -> bevy::ecs::schedule::SystemConfigs;
    fn enter(ec: &mut EntityCommands);
    fn exit(ec: &mut EntityCommands);
}

pub type ActivityId = u8;

#[derive(Deref)]
pub struct ActivityList(pub(crate) Vec<SystemConfigs>);

pub trait ActivitySeq: Send + Sync + 'static {
    fn init(world: &mut World);
    fn into_activity_list() -> ActivityList;
    fn enter(index: ActivityId, ec: &mut EntityCommands);
    fn exit(index: ActivityId, ec: &mut EntityCommands);
    fn type_path() -> &'static str;
    fn short_type_path() -> &'static str;
}

all_tuples!(impl_activity_seq, 1, 20, S);

#[cfg(test)]
mod tests {
    use bevy::prelude::Component;

    use crate::prelude::*;

    use super::*;

    #[derive(Component, Default, Reflect)]
    #[activity(s)]
    struct Test;

    fn s<const A: ActivityId, S>() {}

    #[test]
    fn test_scorer_list() {
        let activities = <(Test, Test)>::into_activity_list();
        assert_eq!(activities.len(), 2);
    }
}
