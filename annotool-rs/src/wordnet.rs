use std::collections::HashMap;

#[derive(Debug,Serialize,Deserialize)]
pub struct WordNet(HashMap<String, WordNetEntry>);

#[derive(Debug,Serialize,Deserialize)]
pub struct WordNetEntry {
    parents : Vec<String>,
    lemmas : Vec<String>,
    defn : String,
    is_instance : bool
}

impl WordNet {
    pub fn lemmas(&self, id : &str) -> String {
        match self.0.get(id) {
            Some(wne) => {
                wne.lemmas.join(", ")
            },
            None =>
                "".to_string()
        }
    }

    pub fn defn(&self, id : &str) -> String {
        match self.0.get(id) {
            Some(wne) => wne.defn.clone(),
            None => "".to_string()
        }
    }
}
