use std::collections::HashMap;

#[derive(Debug,Serialize,Deserialize)]
pub struct Wiki {
    articles : HashMap<String, WikiArticle>,
    categories : HashMap<String, WikiCategory>
}

#[derive(Debug,Serialize,Deserialize)]
pub struct WikiArticle {
    alternatives : Vec<String>,
    categories : Vec<String>
}

#[derive(Debug,Serialize,Deserialize)]
pub struct WikiCategory {
    categories : Vec<String>
}

