use bevy::prelude::*;

#[derive(Debug, Default, Reflect, Clone, Copy)]
pub enum ResponseCurve {
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

impl ResponseCurve {
    pub fn eval(&self, t: f32) -> f32 {
        match *self {
            ResponseCurve::Identity => t,
            ResponseCurve::InverseLerp { min, max } => ((t - min) / (max - min)).clamp(0., 1.),
            ResponseCurve::Smoothstep { min, max } => {
                let t = ((t - min) / (max - min)).clamp(0.0, 1.0);
                t * t * (3.0 - 2.0 * t)
            }
            ResponseCurve::Sigmoid { steepness, center } => {
                1.0 / (1.0 + (-steepness * (t - center)).exp())
            }
            ResponseCurve::Exponential { min, max, exponent } => {
                let t = ((t - min) / (max - min)).clamp(0.0, 1.0);
                t.powf(exponent)
            }
        }
    }
}
