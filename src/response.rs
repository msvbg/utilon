use bevy::prelude::*;

#[derive(Debug, Default, Reflect, Clone, Copy, PartialEq)]
#[repr(C)]
pub enum Response {
    #[default]
    Identity,
    InverseLerp {
        min: f32,
        max: f32,
    },
    Smoothstep {
        min: f32,
        max: f32,
    },
    Sigmoid {
        steepness: f32,
        center: f32,
    },
    Exponential {
        min: f32,
        max: f32,
        exponent: f32,
    },
}

impl Response {
    pub fn eval(&self, t: f32) -> f32 {
        match *self {
            Response::Identity => t,
            Response::InverseLerp { min, max } => ((t - min) / (max - min)).clamp(0., 1.),
            Response::Smoothstep { min, max } => {
                let t = ((t - min) / (max - min)).clamp(0.0, 1.0);
                t * t * (3.0 - 2.0 * t)
            }
            Response::Sigmoid { steepness, center } => {
                1.0 / (1.0 + (-steepness * (t - center)).exp())
            }
            Response::Exponential { min, max, exponent } => {
                let t = ((t - min) / (max - min)).clamp(0.0, 1.0);
                t.powf(exponent)
            }
        }
    }
}
