use std::collections::{HashMap,HashSet};
use std::io::{BufReader,BufRead,BufWriter,Write,Error};
use std::fs::File;
use std::path::Path;
use time::{now,strftime};

struct StringCache(HashMap<String, u32>, Vec<String>);

impl StringCache {
    fn new() -> StringCache {
        StringCache(HashMap::new(), Vec::new())
    }

    fn get_str(&mut self, s : &str) -> u32 {
        if self.0.contains_key(s) {
            return self.0[s];
        } else {
            let i = self.0.len() as u32;
            self.0.insert(s.to_string(), i);
            self.1.push(s.to_string());
            i
        }
    }

    fn get_int(&self, i : u32) -> String { self.1[i as usize].clone() }
    
    fn contains(&self, s : &str) -> bool {
        self.0.contains_key(s)
    }
}

//static ACCEPTED : &'static str = "accepted.tsv";
//static MAPPINGS : &'static str = "../unambiguous-mappings.tsv";
//static LOG : &'static str = "log.txt";

pub struct Mappings {
    cache : StringCache,
    data : HashMap<u64,HashSet<u64>>,
    data_wt : HashMap<u64,i32>,
    by_wn_id : HashMap<String,HashSet<u64>>,
    by_wiki_id : HashMap<String,HashSet<u64>>,
    accepted : HashSet<u64>,
    data_sorted : Vec<u64>,
    log : Vec<String>,
    accepted_file : String,
    mappings_file : String,
    log_file : String
}

static ZERO_I32 : i32 = 0;

impl Mappings {
    pub fn new(accepted_file : &str, mappings_file : &str, log_file : &str) -> Result<Mappings,String> {
        let mut m = Mappings {
            cache : StringCache::new(),
            data  : HashMap::new(),
            data_wt : HashMap::new(),
            by_wn_id : HashMap::new(),
            by_wiki_id : HashMap::new(),
            accepted : HashSet::new(),
            data_sorted : Vec::new(),
            log : Vec::new(),
            accepted_file : accepted_file.to_string(),
            mappings_file : mappings_file.to_string(),
            log_file : log_file.to_string()
        };
        try!(m.load_data());
        Ok(m)
    }

    fn log_msg(&mut self, msg : String) {
        let time_str = strftime("%d/%m/%y %R", &now()).
            unwrap_or_else(|_| String::new());
        println!("[{}] {}", time_str, msg);
        self.log.push(format!("[{}] {}", time_str, msg));
    }

    fn encode(&mut self, s : &str, t : &str) -> u64 {
        ((self.cache.get_str(s) as u64) << 32) + (self.cache.get_str(t) as u64)
    }

    fn decode(&self, l : u64) -> (String, String) {
        (self.cache.get_int((l >> 32) as u32), self.cache.get_int(l as u32))
    }

    fn load_data(&mut self) -> Result<(),String> {
        self.data.clear();

        let f = try!(File::open(&self.mappings_file).map_err(|e| e.to_string()));
        let file = BufReader::new(&f);

        for _line in file.lines() {
            let line = try!(_line.map_err(|e| e.to_string()));
            let mut e = line.split("\t");
            let wt_str = try!(e.next().ok_or("bad_line"));
            let wt = try!(wt_str.parse::<i32>().map_err(|e| format!("Bad weight, possible format error for mapppings: {}", e)));
            let wn_cat = try!(e.next().ok_or("bad line"));
            let wiki_cat = try!(e.next().ok_or("bad line"));
            let wiki_id = try!(e.next().ok_or("bad line"));
            let wn_id = try!(e.next().ok_or("bad line"));
            
            let l = self.encode(wn_cat, wiki_cat);
            if !self.data.contains_key(&l) {
                self.data.insert(l, HashSet::new());
            }
            let l2 = self.encode(wiki_id, wn_id);
            self.data.get_mut(&l).unwrap().insert(l2); // Safe as we insert above

            self.data_wt.insert(l, wt);

            if !self.by_wn_id.contains_key(wn_id) {
                self.by_wn_id.insert(wn_id.to_string(), HashSet::new());
            }
            self.by_wn_id.get_mut(wn_id).unwrap().insert(l);
            
            if !self.by_wiki_id.contains_key(wiki_id) {
                self.by_wiki_id.insert(wiki_id.to_string(), HashSet::new());
            }
            self.by_wiki_id.get_mut(wiki_id).unwrap().insert(l);
        }

        if Path::new(&self.accepted_file).exists() {
            let f2 = try!(File::open(&self.accepted_file).map_err(|e| e.to_string()));
            let file2 = BufReader::new(&f2);
            for _line in file2.lines() {
                let line = try!(_line.map_err(|e| e.to_string()));
                let mut e = line.split("\t");
                let wiki_id = try!(e.next().ok_or("bad line"));
                let wn_id = try!(e.next().ok_or("bad line"));
                
                let l = self.encode(wiki_id, wn_id);
                self.accepted.insert(l);
            }
        }

        self.data_sorted = Vec::new();
        self.data_sorted.extend(self.data.keys());
        self.resort();
        if Path::new(&self.log_file).exists() {
            match File::open(&self.log_file) {
                Ok(f3) => {
                    let file3 = BufReader::new(&f3);
                    for _line in file3.lines() {
                        let line = try!(_line.map_err(|e| e.to_string()));
                        self.log.push(line);
                    }
                },
                Err(_) => {}
            }
        }

        Ok(())
    }
    
    pub fn get_top_mapping(&self) -> Option<(String, String)> {
        self.data_sorted.get(0).map(|l| self.decode(*l))
    }

    pub fn get_mapping(&mut self, wn_cat : &str, wiki_cat : &str) -> Vec<(String, String)> {
        let l = self.encode(wn_cat, wiki_cat);
        self.log_msg(format!("get({},{})", wn_cat, wiki_cat));
        match self.data.get(&l) {
            Some(s) => s.iter()
                .map(|l| self.decode(*l)).collect::<Vec<(String,String)>>(),
            None => Vec::new()
        }
    }

    pub fn reject_cat_map(&mut self, wn_cat : &str, wiki_cat : &str) {
        self.log_msg(format!("reject_cat({},{})", wn_cat, wiki_cat));
        let l = self.encode(wn_cat, wiki_cat);
        self.data.remove(&l);
        self.data_sorted.remove_item(&l);
    }

    pub fn reject_wiki_cat(&mut self, wiki_cat : &str) {
        self.log_msg(format!("reject_wiki_cat({})", wiki_cat));
        let i = self.cache.get_str(&wiki_cat);
        self.data.retain(|l,_|  (*l as u32) != i);
        self.data_sorted.retain(|l| (*l as u32) != i);
    }

    pub fn accept(&mut self, wn_cat : &str, wiki_cat : &str, maps : HashSet<(String, String)>) {
        let n = maps.len();
        let m = self.get_mapping(wn_cat, wiki_cat).len();
        self.log_msg(format!("accept({},{},{}/{})", wn_cat, wiki_cat, n,m));
        for (wiki_id, wn_id) in maps {
            let l = self.encode(&wiki_id, &wn_id);
            self.accepted.insert(l);
            let i = self.cache.get_str(&wn_id);
            match self.by_wn_id.get(&wn_id) {
                Some(s) => {
                    for l2 in s {
                        match self.data.get_mut(&l2) {
                            Some(s2) => {
                                let old_size = s2.len() as i32;
                                if old_size > 0 {
                                    s2.retain(|l3| (*l3 as u32) != i);
                                    let new_size = s2.len() as i32;
                                    let new_score = self.data_wt.get(l2)
                                        .unwrap_or(&ZERO_I32) * new_size / old_size;
                                    self.data_wt.insert(*l2, new_score);
                                }
                                    
                            },
                            None => {}
                        }
                    }
                },
                None => {}
            }
            let j = self.cache.get_str(&wiki_id);
            match self.by_wiki_id.get(&wiki_id) {
                Some(s) => {
                    for l2 in s {
                        match self.data.get_mut(&l2) {
                            Some(s2) => {
                                let old_size = s2.len() as i32;
                                if old_size > 0 {
                                    s2.retain(|l3| (((*l3) >> 32) as u32) != j);
                                    let new_size = s2.len() as i32;
                                    let new_score = self.data_wt.get(l2)
                                        .unwrap_or(&ZERO_I32) * new_size / old_size;
                                    self.data_wt.insert(*l2, new_score);
                                }
                             },
                            None => {}
                        }
                    }
                },
                None => {}
            }
        }
    }

    pub fn resort(&mut self) {
        let ref mut d = self.data_wt;
        self.data_sorted.sort_by(|a,b| d[a].cmp(&d[b]).reverse());
    }

    pub fn save(&self) -> Result<(),Error> {
        {
            let f = try!(File::create(&self.mappings_file));
            let mut out = BufWriter::new(f);
            for (l, m) in self.data.iter() {
                for l2 in m {
                    let (wn_cat, wiki_cat) = self.decode(*l);
                    let (wiki_id, wn_id) = self.decode(*l2);
                    let wt = self.data_wt.get(l).map(|i| *i).unwrap_or(-10000);
                    try!(writeln!(&mut out, "{}\t{}\t{}\t{}\t{}", 
                                  wt, wn_cat, wiki_cat, wiki_id, wn_id));
                }
            }
        }
        {
            let f = try!(File::create(&self.accepted_file));
            let mut out = BufWriter::new(f);
            for l in self.accepted.iter() {
                let (wiki_id, wn_id) = self.decode(*l);
                try!(writeln!(&mut out, "{}\t{}", wiki_id, wn_id));
            }
        }
        {
            let f = try!(File::create(&self.log_file));
            let mut out = BufWriter::new(f);
            for l in self.log.iter() {
                try!(writeln!(&mut out, "{}", l));
            }
        }
        Ok(())
    }

    pub fn accepted_total(&self) -> usize {
        self.accepted.len()
    }

    pub fn contains(&self, wn_or_wiki : &str) -> bool {
        self.cache.contains(wn_or_wiki)
    }
}
