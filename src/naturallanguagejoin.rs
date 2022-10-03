pub trait NaturalLanguageJoin {
    fn natural_language_join(&self) -> String;
}

impl NaturalLanguageJoin for Vec<&'static str> {
    fn natural_language_join(&self) -> String {
        let len = self.len();
        let mut out = String::new();
        let mut i = 0;
        for item in self {
            out.push_str(item);
            i += 1;
            if i < len - 1 {
                out.push_str(", ");
            } else if i == len - 1  {
                out.push_str(" or ");
            }
        }
        out
    }
}
