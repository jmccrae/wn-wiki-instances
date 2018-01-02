#![feature(vec_remove_item)]
#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate clap;
extern crate rocket;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate handlebars;
extern crate time;
extern crate bzip2;
extern crate regex;

mod wordnet;
mod mappings;
mod resources;

use clap::{Arg, App};
use std::collections::{HashMap,HashSet};
use std::sync::RwLock;
use mappings::Mappings;
use std::fs::File;
use rocket::State;
use rocket::Config;
use rocket::config::Environment;
use rocket::request::{FromForm, FormItems, Form};
use rocket::response::{Content,Redirect};
use rocket::http::ContentType;
use handlebars::Handlebars;
use bzip2::read::BzDecoder;
use std::io::prelude::*;
use std::io::BufReader;
use regex::Regex;

#[derive(Debug,Clone,Serialize,Deserialize)]
struct AnnoToolData {
    wn_cat : String,
    wne : WNE,
    wiki_cat : String,
    mapping : Vec<AnnoMapping>,
    context : String
}

#[derive(Debug,Clone,Serialize,Deserialize)]
struct WNE {
    lemmas : String,
    defn : String
}

#[derive(Debug,Clone,Serialize,Deserialize)]
struct AnnoMapping {
    wn_id : String,
    wiki_id : String,
    lemma : String,
    defn : String,
    wiki_defn : String,
    good : bool
}

#[derive(Debug,Clone,Serialize,Deserialize)]
struct Page {
    context: String,
    body: String,
    accepted_total: usize
}


struct WordNetWikiMapping {
    wordnet : wordnet::WordNet,
    mappings : mappings::Mappings,
    handlebars : Handlebars,
    context : String,
    abstracts: HashMap<String, String>
}

#[get("/assets/bootstrap.min.css")]
fn bootstrap() -> Content<&'static str> { 
    Content(ContentType::CSS, resources::BOOTSTRAP)
}

#[get("/assets/jquery-1.12.0.min.js")]
fn jquery() -> Content<&'static str> {
    Content(ContentType::JavaScript, resources::JQUERY)
}

fn load_short_abstracts(abstracts_file : &str, mappings : &Mappings) 
        -> Result<HashMap<String,String>,String> {
    let line_regex = Regex::new("<http.*resource/(.*)> <http.*comment> \"(.*)\"@en \\..*")
        .expect("Abstract regex did not compile");
    let file = File::open(abstracts_file)
        .map_err(|e| format!("Could not open abstracts file: {}", e))?;
    let reader = BufReader::new(BzDecoder::new(file));

    let mut abstracts = HashMap::new();

    for _line in reader.lines() {
        let line = _line.map_err(|e| format!("Could not read abstracts file: {}", e))?;
        match line_regex.captures(&line) {
            Some(caps) => {
                let key = caps.get(1).map(|s| s.as_str()).unwrap_or("")
                    .replace("_", " ");
                
                let abs = caps.get(2).map(|s| s.as_str()).unwrap_or("").to_string();

                if mappings.contains(&key) {
                    abstracts.insert(key,abs);
                }
            },
            None => {
                //eprintln!("{}", line);
            }
        }
    }
    Ok(abstracts)
}



fn pack_data(wn_cat : &str, wiki_cat : &str, state : &mut WordNetWikiMapping) -> AnnoToolData {
    let m = state.mappings.get_mapping(&wn_cat, &wiki_cat);
    let mut mapping : Vec<AnnoMapping> = m.iter().map(|x| {
            let &(ref wiki_id, ref wn_id) = x;
            AnnoMapping {
                wn_id: wn_id.clone(),
                wiki_id: wiki_id.clone(),
                lemma: state.wordnet.lemmas(&wn_id),
                defn: state.wordnet.defn(&wn_id),
                wiki_defn: state.abstracts.get(wiki_id).map(|s| s.to_string()).unwrap_or(String::new()),
                good: state.mappings.is_good(wn_cat, wiki_cat)
            }
        }).collect();
    mapping.sort_by(|a,b| a.wiki_id.cmp(&b.wiki_id));
    AnnoToolData {
        wn_cat : wn_cat.to_string(),
        wne : WNE {
            lemmas : state.wordnet.lemmas(wn_cat),
            defn : state.wordnet.defn(wn_cat)
        },
        wiki_cat : wiki_cat.to_string(),
        mapping : mapping,
        context : state.context.clone()
    }
}

#[get("/")]
fn index(_state : State<RwLock<WordNetWikiMapping>>) -> Result<Content<String>,String> {
    let mut state =try!( _state.write().map_err(|_| "Restart server!"));
    match state.mappings.get_top_mapping() {
        Some((wn_cat, wiki_cat)) => {
            let data = pack_data(&wn_cat, &wiki_cat, &mut state);
            let body = try!(state.handlebars.render("anno-tool", &data)
                            .map_err(|e| e.to_string()));
            let page = try!(state.handlebars.render("layout", &Page { 
                context: state.context.clone(), 
                body:body, 
                accepted_total: state.mappings.accepted_total()
            }).map_err(|e| e.to_string()));
            Ok(Content(ContentType::HTML, page))
        },
        None => {
            let page = try!(state.handlebars.render("layout", &Page { 
                context: state.context.clone(), 
                body: "All done!".to_string(),
                accepted_total: state.mappings.accepted_total()
            }).map_err(|e| e.to_string()));
            Ok(Content(ContentType::HTML, page))
        }
    }
}

struct ParamMap(HashMap<String, Vec<String>>);

impl<'f> FromForm<'f> for ParamMap {
    type Error = String;
    fn from_form(it : &mut FormItems<'f>, _ :bool) -> Result<Self, Self::Error> {
        let mut m : HashMap<String, Vec<String>> = HashMap::new();
        for (key, value) in it {
            let k = try!(key.url_decode().map_err(|e| e.to_string()));
            let v = try!(value.url_decode().map_err(|e| e.to_string()));
            m.entry(k).or_insert(Vec::new()).push(v.to_string());
        }
        Ok(ParamMap(m))
    }
}

impl ParamMap {
    fn get(&self, s : &str) -> Option<String> {
        match self.0.get(s) {
            Some(v) => if v.len() == 1 {
                Some(v[0].clone())
            } else {
                None
            },
            None => None
        }
    }
    //fn len(&self) -> usize { self.0.len() }
}

#[get("/reject?<params>")]
fn reject(_state : State<RwLock<WordNetWikiMapping>>, params : ParamMap) -> Result<Redirect, String> {
    let mut state =try!( _state.write().map_err(|_| "Restart server!"));
    match params.get("wn_cat").and_then(|wn_cat| {
        params.get("wiki_cat").map(|wiki_cat| {
            state.mappings.reject_cat_map(&wn_cat, &wiki_cat)
        })
    }) {
        Some(()) => Ok(Redirect::to(&format!("{}/", state.context))),
        None => Err("Bad request".to_string())
    }
}

#[get("/reject_cat?<params>")]
fn reject_cat(_state : State<RwLock<WordNetWikiMapping>>, params : ParamMap) -> Result<Redirect, String> {
    let mut state =try!( _state.write().map_err(|_| "Restart server!"));
    match params.get("wiki_cat").map(|wiki_cat| {
        state.mappings.reject_wiki_cat(&wiki_cat);
    }) {
        Some(()) => Ok(Redirect::to(&format!("{}/", state.context))),
        None => Err("Bad Request".to_string())
    }
}

#[get("/accept?<params>")]
fn accept_get(_state : State<RwLock<WordNetWikiMapping>>, params : ParamMap) -> Result<Redirect, String> {
    accept(_state, &params)
}

#[post("/accept", data="<params>")]
fn accept_post(_state : State<RwLock<WordNetWikiMapping>>, params : Form<ParamMap>) -> Result<Redirect, String> {
    accept(_state, params.get())
}

fn accept(_state : State<RwLock<WordNetWikiMapping>>, params : &ParamMap) -> Result<Redirect, String> {
    let mut state =try!( _state.write().map_err(|_| "Restart server!"));
    match params.get("wn_cat").and_then(|wn_cat| {
        params.get("wiki_cat").map(|wiki_cat| {
            let mut accepts = HashSet::new();
            for (k,v) in params.0.iter() {
                if v.iter().all(|x| x.starts_with("wn31")) && k != "wn_cat" {
                    if v.len() == 1 {
                        accepts.insert((k.to_string(),v[0].to_string()));
                    } else {
                        eprintln!("Duplicate {} => {}", k, v.join(","));
                        return Err(format!("Duplicate {} => {}", k, v.join(",")));
                    }
                }
            }
            println!("accept({},{},{})", wn_cat, wiki_cat, accepts.len());
            state.mappings.accept(&wn_cat, &wiki_cat, accepts);
            state.mappings.reject_cat_map(&wn_cat, &wiki_cat);
            state.mappings.reject_wiki_cat(&wiki_cat);
            state.mappings.resort();
            Ok(())
        })
    }) {
        Some(Ok(())) => Ok(Redirect::to(&format!("{}/", state.context))),
        Some(Err(msg)) => Err(msg),
        None => Err("Bad request".to_string())
    }
}

#[get("/save")]
fn save(_state : State<RwLock<WordNetWikiMapping>>) -> Result<Redirect,String> {
    let state = try!(_state.read().map_err(|_| "Restart server!"));
    state.mappings.save()
        .map(|_| Redirect::to(&format!("{}/", state.context)))
        .map_err(|e| e.to_string())
}
    


fn main() {
    let args = App::new("WordNet Wikipedia Annotation Tool")
        .version("0.1")
        .author("John P. McCrae <john@mccr.ae>")
        .about("Tool used for annotating mappings between WordNet instances and Wikipedia")
        .arg(Arg::with_name("port")
             .short("p")
             .long("port")
             .value_name("PORT")
             .help("The port to run the server on")
             .takes_value(true))
        .arg(Arg::with_name("wordnet")
             .long("wordnet")
             .value_name("wordnet.json")
             .help("The location of the WordNet JSON file (from PreprocessWordNet.scala)")
             .takes_value(true))
        .arg(Arg::with_name("mappings")
             .long("mappings")
             .value_name("unambiguous-mappings.tsv")
             .help("The unambibiguous mappings (from FindUnambiguousMappings.scala)")
             .takes_value(true))
        .arg(Arg::with_name("accepted")
             .long("accepted")
             .value_name("accepted.tsv")
             .help("The file to write accepted mappings to")
             .takes_value(true))
        .arg(Arg::with_name("log")
             .long("log")
             .value_name("log.txt")
             .help("Where to write the log file to")
             .takes_value(true))
        .arg(Arg::with_name("context")
             .short("c")
             .long("context")
             .help("The full path of the server, e.g., http://example.com/tool")
             .takes_value(true))
        .arg(Arg::with_name("abstracts")
             .short("a")
             .long("abstracts")
             .help("The set of DBpedia short abstracts, e.g., http://downloads.dbpedia.org/2016-10/core-i18n/en/short_abstracts_en.ttl.bz2")
             .takes_value(true))
        .get_matches();
        
    let port = args.value_of("port")
        .and_then(|pstr| { pstr.parse::<u16>().ok() })
        .unwrap_or(8000);

    let wordnet_file = args.value_of("wordnet")
        .unwrap_or("wordnet.json");

    let mappings_file = args.value_of("mappings")
        .unwrap_or("unambiguous-mappings.tsv");

    let accepted_file = args.value_of("accepted")
        .unwrap_or("accepted.tsv");

    let log_file = args.value_of("log")
        .unwrap_or("log.txt");

    let context = args.value_of("context")
        .unwrap_or("");

    let abstracts_file = args.value_of("abstracts");
    
    println!("Loading WordNet");
    let wn_file = File ::open(wordnet_file).expect("wordnet.json does not exist");
    let wordnet = serde_json::from_reader(wn_file).expect("could not parse wordnet json");
    println!("Loading mappings");
    let mappings = Mappings::new(accepted_file, mappings_file, log_file).unwrap();
    println!("Loading abstracts");
    let abstracts = match abstracts_file {
        Some(f) =>  load_short_abstracts(&f, &mappings).expect("Could not load abstracts"),
        None => HashMap::new()
    };
    println!("Loading templates");
    let mut hbars = Handlebars::new();
    hbars.register_template_string("layout", resources::LAYOUT).expect("Failed to load layout");
    hbars.register_template_string("anno-tool", resources::ANNO_TOOL).expect("Failed to load anno tool");
    rocket::custom(Config::build(Environment::Production)
                   .port(port)
                   .finalize().expect("Could not configure Rocket"), true)
        .manage(RwLock::new(WordNetWikiMapping {
            wordnet: wordnet,
            mappings: mappings,
            handlebars: hbars,
            abstracts: abstracts,
            context: context.to_string()
        }))
        .mount("/", routes![index,bootstrap,jquery,reject,reject_cat,accept_get,accept_post,save]).launch();
}
