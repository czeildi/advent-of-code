const fs = require('fs');

let data;
data = fs.readFileSync('solutions_2021/day01_input.txt', 'utf8').split('\n')

data = data.map(depth => parseInt(depth, 10))

// part 1

let num_increased = 0;

for (let i = 1; i < data.length; i++) {
  if (data[i] > data[i - 1]) num_increased += 1;
}

console.log(num_increased)

// part 2

let num_window_increased = 0;

for (let i = 1; i < data.length; i++) {
  if (data[i + 2] > data[i - 1]) num_window_increased += 1;
}

console.log(num_window_increased)
