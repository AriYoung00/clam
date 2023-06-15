import function_name from script.sh

let files: list<string> = `ls *.c`;
let libs: list<string> = ["pthread", "libxml2"];
let pkg_args: string = `pkg-config --cflags --libs %{libs}`;

`gcc ${pkg_args} ${files}`;

let output_lines: list<string> = ```sh
ls -l | grep "criteria" | wc -l
```

let some_bash_script = ```bash
# initialize a variable
count=0

# loop through a sequence of numbers
for i in {1..5}; do
  # check if the current number is even
  if (( i % 2 == 0 )); then
    echo "$i is even"
  else
    echo "$i is odd"
  fi
  
  # increment the counter variable
  ((count++))
done

# print the final count
echo "Looped $count times"

```;
assert_eq(some_bash_script, ["1 is odd", "2 is even", "3 is odd", "4 is even", "5 is odd", "Looped 5 times"]);

fn test(i: int) -> int {
  i + 1
}

if true {
  // do stuff
}

struct A {
  s: string;
  n: u32;

  fn a(s: string, n: u32) {
    // do stuff
  }
}

impl A {
    fn a(s: string, n: u32) {
        k
    }
}
