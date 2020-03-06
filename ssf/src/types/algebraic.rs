use super::constructor::Constructor;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Algebraic {
    constructors: Vec<Constructor>,
}

impl Algebraic {
    pub fn new(constructors: Vec<Constructor>) -> Self {
        if constructors.is_empty() {
            panic!("no constructors in algebraic data type");
        }

        Self { constructors }
    }

    pub fn constructors(&self) -> &[Constructor] {
        &self.constructors
    }

    pub fn is_singleton(&self) -> bool {
        self.constructors.len() == 1
    }

    pub fn is_enum(&self) -> bool {
        self.constructors
            .iter()
            .all(|constructor| constructor.is_enum())
    }

    pub fn to_id(&self) -> String {
        format!(
            "{{{}}}",
            self.constructors
                .iter()
                .map(|constructor| constructor.to_id())
                .collect::<Vec<_>>()
                .join(","),
        )
    }

    pub fn unfold(&self) -> Self {
        Self {
            constructors: self
                .constructors
                .iter()
                .map(|constructor| constructor.unfold(self))
                .collect(),
        }
    }

    pub(crate) fn unfold_with(&self, algebraic_type: &Self) -> Self {
        Self {
            constructors: self
                .constructors
                .iter()
                .map(|constructor| constructor.unfold(algebraic_type))
                .collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::primitive::Primitive;
    use super::*;

    #[test]
    #[should_panic]
    fn new_with_no_constructor() {
        Algebraic::new(vec![]);
    }

    #[test]
    fn to_id() {
        assert_eq!(
            &Algebraic::new(vec![Constructor::boxed(vec![])]).to_id(),
            "{{}}"
        );
        assert_eq!(
            &Algebraic::new(vec![Constructor::boxed(vec![]), Constructor::boxed(vec![])]).to_id(),
            "{{},{}}"
        );
        assert_eq!(
            &Algebraic::new(vec![
                Constructor::boxed(vec![]),
                Constructor::boxed(vec![Primitive::Float64.into()])
            ])
            .to_id(),
            "{{},{Float64}}"
        );
        assert_eq!(
            &Algebraic::new(vec![
                Constructor::boxed(vec![Primitive::Float64.into()]),
                Constructor::boxed(vec![])
            ])
            .to_id(),
            "{{Float64},{}}"
        );
    }
}
