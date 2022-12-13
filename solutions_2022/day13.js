'use strict';

const assert = require('assert');
const fs = require('fs');
const { tail, sum, isEqual } = require('lodash');

const pairs = fs.readFileSync('./solutions_2022/day13_input.txt', 'utf8').split('\n\n')

const compare = (left, right) => {
  if (left.length === 0 & right.length === 0) return 0

  if (left.length === 0 | right.length === 0) {
    return left.length < right.length ? -1 : 1;
  }
  const leftValue = left[0]
  const rightValue = right[0]

  if (!Array.isArray(leftValue) & !Array.isArray(rightValue)) {
    if (leftValue === rightValue) {
      return compare(tail(left), tail(right))
    }
    return leftValue < rightValue ? -1 : 1;
  }
  if (Array.isArray(leftValue) & Array.isArray(rightValue)) {
    const res = compare(leftValue, rightValue)
    if (res === 0) {
      return compare(tail(left), tail(right))
    }
    return res
  }
  if (Array.isArray(leftValue) & !Array.isArray(rightValue)) {
    const res = compare(leftValue, [rightValue])
    if (res === 0) {
      return compare(tail(left), tail(right))
    }
    return res
  }
  if (!Array.isArray(leftValue) & Array.isArray(rightValue)) {
    const res = compare([leftValue], rightValue)
    if (res === 0) {
      return compare(tail(left), tail(right))
    }
    return res
  }
}

assert(compare([], []) === 0)
assert(compare([], [[]]) === -1)
assert(compare([], [1]) === -1)
assert(compare([[1]], [[1]]) === 0)

// part 1
const res = pairs.map((pair, idx) => {
  const [leftRaw, rightRaw] = pair.split('\n')
  const [left, right] = [leftRaw, rightRaw].map(JSON.parse)
  // console.log(left, '\n', right)
  
  return [idx, compare(left, right)]
})

console.log(sum(res.filter(r => r[1] === -1).map(r => r[0] + 1)))

// part 2
const elements = [[[2]], [[6]]]

for (const pair of pairs) {
  const [leftRaw, rightRaw] = pair.split('\n')
  elements.push(JSON.parse(leftRaw))
  elements.push(JSON.parse(rightRaw))
}

const sorted = elements.sort(compare)

const index_2 = sorted.findIndex(val => isEqual(val, [[2]])) + 1
const index_6 = sorted.findIndex(val => isEqual(val, [[6]])) + 1
console.log(index_2 * index_6)
