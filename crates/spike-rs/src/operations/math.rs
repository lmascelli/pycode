pub fn mean(range: &[f32]) -> f32 {
    let mut sum = 0f32;
    range.iter().for_each(|x| {
        sum += x;
    });
    sum / (range.len() as f32)
}

pub fn stdev(range: &[f32]) -> f32 {
    let mut sum = 0f32;
    let _mean = mean(range);
    range.iter().for_each(|x| {
        sum += (x - _mean) * (x - _mean);
    });
    (sum / (range.len() as f32 - 1.0f32)).sqrt()
}

pub fn min(range: &[f32]) -> f32 {
    let mut min = range[0];
    for value in range {
        if *value < min {
            min = *value;
        }
    }
    min
}

pub fn max(range: &[f32]) -> f32 {
    let mut max = range[0];
    for value in range {
        if *value > max {
            max = *value;
        }
    }
    max
}

pub fn train(n: usize, step: usize, offset: usize) -> Vec<f32> {
    let mut ret = vec![0f32; step * n + offset];
    for i in 0..n {
        ret[i * step + offset] = 1f32;
    }
    ret
}

pub fn convolve(s1: &[f32], s2: &[f32]) -> Vec<f32> {
    let (signal, filter) = if s1.len() > s2.len() {
        (s1, s2)
    } else {
        (s2, s1)
    };
    // let filter: Vec<f32> = filter.iter().rev().map(|x| *x).collect();
    let slen = signal.len();
    let flen = filter.len();

    let mut ret = vec![0f32; slen];

    // head
    for i in 0..flen {
        for j in (0..i).rev() {
            ret[i] += signal[i - j] * filter[j];
        }
    }

    // body
    for i in flen..(slen - flen) {
        for j in 0..flen {
            ret[i] += signal[i - j] * filter[j];
        }
    }

    // tail
    for i in (slen - flen)..slen {
        for j in slen - i..=0 {
            ret[i] += signal[i - j] * filter[j];
        }
    }

    ret
}

pub fn diff<T>(data: &[T]) -> Vec<T>
where
    T: Default + Copy + std::ops::Sub<Output = T>,
{
    let mut ret = vec![T::default(); data.len() - 1];

    for i in 0..data.len() - 1 {
        ret[i] = data[i + 1] - data[i];
    }

    ret
}

pub fn logspace(start: f32, end: f32, n_points: usize) -> Vec<f32> {
    let mut ret = vec![0f32; n_points];

    let step = (end - start) / (n_points - 1) as f32;
    for i in 0..n_points {
        let exp = start + (i as f32 * step);
        let val = 10f32.powf(exp);
        ret[i] = val;
    }

    ret
}
