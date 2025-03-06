/// Find the peaks that fall in the interval [point - window_half_size, point + window_half_size]
/// for each point passed. For optimization purposes it assume that the peaks and the points are
/// sorted in ascending order. It return the indices of the found peaks.

pub fn find_peaks_around_points(
    peak_times: &[usize],
    points: &[usize],
    window_half_size: usize,
) -> Vec<usize> {
    let mut indices = vec![];
    let mut peak_index = 0;
    let mut point_index = 0;
    if points.len() == 0 {
        return vec![];
    }
    if points[0] < window_half_size {
        while peak_index < peak_times.len() && peak_times[peak_index] < points[0] {
            indices.push(peak_times[peak_index]);
            peak_index += 1;
        }
        point_index += 1;
    }

    while peak_index < peak_times.len() && point_index < points.len() {
        let peak_time = peak_times[peak_index];
        let point = points[point_index];
        if peak_time < point - window_half_size {
            peak_index += 1;
        } else if peak_time >= point - window_half_size && peak_time <= point + window_half_size {
            indices.push(peak_index);
            peak_index += 1;
        } else {
            point_index += 1;
        }
    }

    indices
}
