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
