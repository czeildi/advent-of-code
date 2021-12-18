'use strict';

const fs = require('fs');

const magnitude = fishNumber => {
  if (!Array.isArray(fishNumber)) return fishNumber;
  return 3 * magnitude(fishNumber[0]) + 2 * magnitude(fishNumber[1]);
};

// part 1

const totalMagnitude = magnitude(
  JSON.parse(
    fs.readFileSync('./solutions_2021/day18_intermediate_p1.txt', 'utf8').split('\n')[0]
  )
);
console.log('part1', totalMagnitude);

// part 2
const data = fs.readFileSync('./solutions_2021/day18_intermediate.txt', 'utf8').split('\n')

const maxMagnitude = Math.max(...data.map(final => final !== '' ? magnitude(JSON.parse(final)) : 0));

console.log('part2', maxMagnitude);