pub enum ButterworthFilterType {
    HighPass,
    LowPass,
}

#[derive(Debug)]
pub struct ButterworthFilter {
    pub order: usize,
    pub b: Vec<f32>,
    pub a: Vec<f32>,
    pub zi: Vec<f32>,
}

impl ButterworthFilter {
    pub fn create(
        filter_type: ButterworthFilterType,
        order: usize,
        cutoff_frequency: f32,
        sample_rate: f32,
    ) -> Self {
        let cutoff_frequency = match filter_type {
            ButterworthFilterType::HighPass => sample_rate / 2f32 - cutoff_frequency,
            ButterworthFilterType::LowPass => cutoff_frequency,
        };
        let omega_c = 2f32 * std::f32::consts::PI * cutoff_frequency / sample_rate;
        let sn = omega_c.sin();
        let cs = omega_c.cos();

        let mut s1 = vec![0f32; order];
        let mut s2 = vec![0f32; order];

        for i in 0..order {
            let theta = std::f32::consts::PI * (2f32 * i as f32 + 1f32) / (2f32 * order as f32);
            s1[i] = -1f32 * theta.sin();
            s2[i] = theta.cos();
        }

        let mut bz = vec![0f32; order + 1];
        let mut az = vec![0f32; order + 1];

        bz[0] = 1f32;
        az[0] = 1f32;

        for i in 0..order {
            let alpha = sn * s1[i];
            let beta = cs;

            let denom = 1f32 + alpha;
            let b0 = sn * sn / (4f32 * denom);
            let b1 = sn * sn / (2f32 * denom);
            let b2 = b0;
            let a1 = 2f32 * (beta - alpha) / denom;
            let a2 = (1f32 - alpha) / denom;

            let mut bz_temp = vec![0f32; order + 1];
            let mut az_temp = vec![0f32; order + 1];

            for j in 0..order {
                let mut k = 0;
                while k <= 2 && j + k <= order {
                    bz_temp[j + k] += bz[j]
                        * match k {
                            0 => b0,
                            1 => b1,
                            2 => b2,
                            _ => unreachable!(),
                        };
                    az_temp[j + k] += az[j]
                        * match k {
                            0 => 1f32,
                            1 => a1,
                            2 => a2,
                            _ => unreachable!(),
                        };
                    k += 1;
                }
            }

            match filter_type {
                ButterworthFilterType::HighPass => {
                    for j in 0..=order {
                        if j % 2 == 1 {
                            bz_temp[j] *= -1.;
                            az_temp[j] *= -1.;
                        }
                    }
                }
                _ => (),
            };

            for j in 0..=order {
                bz[j] = bz_temp[j];
                az[j] = az_temp[j];
            }
        }

        Self {
            order,
            b: bz,
            a: az,
            zi: vec![0f32; 2 * order],
        }
    }
}

pub fn filter_data(filter: &ButterworthFilter, input: &[f32]) -> Vec<f32> {
    let mut output = vec![0f32; input.len()];

    let mut x = vec![0f32; filter.order + 1];
    let mut y = vec![0f32; filter.order + 1];

    for (i, value) in input.iter().enumerate() {
        x[0] = *value;
        y[0] = 0f32;

        for j in 1..=filter.order {
            y[0] += filter.b[j] * x[j] - filter.a[j] * y[j];
        }

        output[i] = y[0];

        let mut j = filter.order;
        while j > 0 {
            x[j] = x[j - 1];
            y[j] = y[j - 1];
            j -= 1;
        }
    }
    output
}

pub fn filtfilt(filter: &ButterworthFilter, input: &[f32]) -> Vec<f32> {
    let datalen = input.len();
    let mut output = vec![0f32; input.len()];
    {
        let temp = filter_data(filter, input);
        for i in 0..datalen {
            output[i] = temp[datalen - 1 - i];
        }
    }
    {
        let temp = filter_data(filter, output[..].as_ref());
        for i in 0..datalen {
            output[i] = temp[datalen - 1 - i];
        }
    }
    output
}

pub fn tricube_weight(distance: f32, samples_fraction: f32, total_samples: usize) -> f32 {
    let span = (total_samples - 1) as f32 * samples_fraction;
    let d = distance.abs() / span;
    if d < 1f32 {
        (1f32 - d.powf(3f32)).powf(3f32)
    } else {
        0f32
    }
}

#[allow(non_snake_case)]
pub fn weighted_linear_regression(x: &[f32], y: &[f32], w: &[f32], x0: f32) -> f32 {
    let mut W = 0f32;
    let mut XW = 0f32;
    let mut YW = 0f32;
    let mut XXW = 0f32;
    let mut XYW = 0f32;

    for i in 0..x.len() {
        W += w[i];
        XW += x[i] * w[i];
        YW += y[i] * w[i];
        XXW += x[i] * x[i] * w[i];
        XYW += x[i] * y[i] * w[i];
    }

    let denominator = W * XXW - XW * XW;
    if denominator == 0f32 {
        return 0f32;
    } else {
        let slope = (W * XYW - XW * YW) / denominator;
        let intercept = (XXW * YW - XW * XYW) / denominator;

        return slope * x0 + intercept;
    }
}

pub fn lowess(data: &[f32], samples_fraction: f32) -> Vec<f32> {
    #[allow(non_snake_case)]
    let N = data.len();
    let mut ret = vec![0f32; N];
    let x: Vec<f32> = (0..N).map(|x| x as f32).collect();
    let mut w = vec![0f32; N];

    for i in 0..N {
        for j in 0..N {
            w[j] = tricube_weight(x[i] - j as f32, samples_fraction, N);
        }
        ret[i] = weighted_linear_regression(x[..].as_ref(), data, w[..].as_ref(), x[i]);
    }

    ret
}
