
#[derive(Debug, Clone)]
struct Row {
    dimension: usize,
    data: Vec<f64>,
}

impl Row {
    pub fn from_cols(data: Vec<f64>) -> Row {
        Row {
            dimension: data.len(),
            data,
        }
    }

    pub fn add(&self, rhs: &Row) -> Self {
        Row {
            dimension: self.dimension,
            data: self.data.iter().enumerate().map(|(i, e)| e + rhs.data[i]).collect()
        }
    }

    pub fn sub(&self, rhs: &Row) -> Self {
        self.add(&rhs.mul(-1.0))
    }

    pub fn mul(&self, rhs: f64) -> Self {
        Row {
            dimension: self.dimension,
            data: self.data.iter().map(|e| e * rhs).collect()
        }
    }
}

#[derive(Debug, Clone)]
struct AugmentedMatrix {
    dimension: usize,
    rows: Vec<Row>,
}

impl AugmentedMatrix {
    pub fn from_rows(rows: Vec<Row>) -> AugmentedMatrix {
        AugmentedMatrix {
            dimension: rows.len(),
            rows,
        }
    }

    pub fn transform(mut self) -> Self {

        for i in 1..self.dimension {
            let pivot = i - 1;

            for b in i..self.dimension {
                let factor = self.rows[b].data[pivot] / self.rows[i - 1].data[pivot];
                let row = self.rows[b].sub(&self.rows[i - 1].mul(factor));

                self.rows[b] = row;
            }
        }

        self
    }

    pub fn solve(mut self) -> Vec<f64> {
        let mut solution_set = Vec::new();

        for (i, row) in self.rows.iter_mut().rev().enumerate() {
            for i in 0..i {
                row.data[self.dimension] += row.data[self.dimension - (1 + i)] * -1.0 * solution_set[i];
            }

            solution_set.push(row.data[self.dimension] / row.data[self.dimension - (1 + i)]);
        }

        solution_set
    }
}



///
/// -x + y + z  = 0
/// 2x + 2y + z = 1
/// 3x + 4y - 2z = 5
/// 
/// 

fn main() {
    let input = AugmentedMatrix::from_rows(vec![
        Row::from_cols(vec![3.0, -1.0, 1.0, 1.0, 0.0]),
        Row::from_cols(vec![2.0, 2.0, 2.0, 1.0, 1.0]),
        Row::from_cols(vec![3.0, 3.0, 4.0, -2.0, 5.0]),
        Row::from_cols(vec![7.0, 4.0, -3.0, -14.0, 17.0])
    ]);

    dbg!(input.transform().solve());
}
