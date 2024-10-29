#![doc = include_str!("../../../README.md")]
// `rustdoc_internals` is needed for `#[doc(fake_variadics)]`
#![allow(internal_features)]
#![cfg_attr(any(docsrs, docsrs_dep), feature(doc_auto_cfg, rustdoc_internals))]

use bevy::{
    app::{App, Plugin, PreUpdate},
    ecs::{intern::Interned, schedule::ScheduleLabel},
    prelude::*,
};

mod activity;
mod behavior;

pub mod prelude {
    pub use crate::{
        activity::{Activity, ActivityId, ActivitySeq},
        behavior::{ActivityState, Behavior, BehaviorSettings, Policy, UtilonAppExt},
        UtilonPlugin,
    };

    pub use utilon_macros::activity;
}

#[derive(Debug, Clone, Copy)]
pub enum StateMode {
    Realtime,
    Manual,
}

#[derive(Resource, Clone)]
pub struct UtilonConfig {
    pub schedule: Interned<dyn ScheduleLabel>,
    pub state_mode: StateMode,
}

impl Default for UtilonPlugin {
    fn default() -> Self {
        Self(UtilonConfig {
            schedule: PreUpdate.intern(),
            state_mode: StateMode::Realtime,
        })
    }
}

#[derive(SystemSet, Debug, Hash, PartialEq, Eq, Clone)]
pub struct ScoringSet;

#[derive(SystemSet, Debug, Hash, PartialEq, Eq, Clone)]
pub struct TransitionSet;

pub struct UtilonPlugin(pub UtilonConfig);

impl Plugin for UtilonPlugin {
    fn build(&self, app: &mut App) {
        let config = &self.0;
        app.insert_resource(config.clone());

        match config.state_mode {
            StateMode::Realtime => {
                app.configure_sets(config.schedule, (ScoringSet, TransitionSet).chain());
            }
            StateMode::Manual => todo!(),
        }
    }
}
