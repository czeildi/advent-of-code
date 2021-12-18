'use strict';

const fs = require('fs');

let data;
data = fs.readFileSync('./solutions_2021/day18_intermediate.txt', 'utf8').split('\n')

const magnitude = fishNumber => {
  if (!Array.isArray(fishNumber)) return fishNumber;
  return 3 * magnitude(fishNumber[0]) + 2 * magnitude(fishNumber[1]);
};

const maxMagnitude = Math.max(...data.map(final => final !== '' ? magnitude(JSON.parse(final)) : 0));

console.log(maxMagnitude);

