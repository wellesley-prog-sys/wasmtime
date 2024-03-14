use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::Path;
use std::env;

fn extract_rule(text: &str) -> Option<&str> {
    let mut open_count = 0; //Counting Parenthesis
    let mut rule_start = None;

    for (i, char) in text.char_indices() {
        match char {
            '(' => {
                open_count += 1;
                if open_count == 1 {
                    rule_start = Some(i);
                }
            }
            ')' => {
                open_count -= 1;
                if open_count == 0 {
                    return Some(&text[rule_start.unwrap()..=i]);
                }
            }
            _ => {}
        }
    }

    None
}
//https://stackoverflow.com/questions/30801031/read-a-file-and-get-an-array-of-strings
fn lines_from_file(filename: impl AsRef<Path>) -> Vec<String> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .collect()
}

fn find_any_elements(string: &str, elements: &[String]) -> Vec<String> {
  let mut matches: Vec<String> = Vec::new();
  for element in elements.iter() {
    if string.contains(element.as_str()) {
      matches.push(element.to_string()); // Clone the matched element
    }
  }
  matches
}

    fn main() {

        let file_path = "construct_and_extract.isle"
        // let args: Vec<String> = env::args().collect();
        // if args.len() < 2 {
        //     println!("Usage: {} <file_path>", args[0]);
        //     return;
        // }
        
        // let file_path = &args[1];
        let content = fs::read_to_string(file_path)
            .expect("Failed to read file");
        // println!("File contents:\n{}", content);
        
        // Find the start of the rule block
        let start_index = match content.find("(rule") {
            Some(index) => index,
            None => {
                println!("No rule block found!");
                return;
            }
        };

        // Extract the rule block
        let text = &content[start_index..];

        let rule_block = extract_rule(text);
        let rule = rule_block.as_ref().unwrap();

        let lines = lines_from_file("instructions.txt");
        let mut rules_list = Vec::new();
        for line in lines {
            rules_list.push(line.clone());
            //println!("{:?}", line);
        }

        let found_elements = find_any_elements(rule, &rules_list);
        println!("{:?}", found_elements);

        if !found_elements.is_empty() {
          println!("Found elements from the array in the string: {:?}", found_elements);
        } else {
          println!("No Rules Found");
        }

}
