use bevy::prelude::Query;

use crate::prelude::{ActivityId, ActivitySeq, Behavior};

pub fn always_one<const A: ActivityId, S: ActivitySeq>(query: Query<&Behavior<S>>) {
    for behavior in query.iter() {
        behavior.score::<A>(|| 1.0);
    }
}

pub fn always_half<const A: ActivityId, S: ActivitySeq>(query: Query<&Behavior<S>>) {
    for behavior in query.iter() {
        behavior.score::<A>(|| 0.5);
    }
}

pub fn always_zero<const A: ActivityId, S: ActivitySeq>(query: Query<&Behavior<S>>) {
    for behavior in query.iter() {
        behavior.score::<A>(|| 0.0);
    }
}
