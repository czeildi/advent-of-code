'use strict';

const assert = require('assert');
const fs = require('fs');
const { tail, sum } = require('lodash');

const pairs = fs.readFileSync('./solutions_2022/day13_input.txt', 'utf8').split('\n\n')

const compare = (left, right) => {
  if (left.length === 0 & right.length === 0) return 'eq'

  if (left.length === 0 | right.length === 0) {
    return left.length < right.length;
  }
  const leftValue = left[0]
  const rightValue = right[0]

  if (!Array.isArray(leftValue) & !Array.isArray(rightValue)) {
    if (leftValue === rightValue) {
      return compare(tail(left), tail(right))
    }
    return leftValue < rightValue
  }
  if (Array.isArray(leftValue) & Array.isArray(rightValue)) {
    const res = compare(leftValue, rightValue)
    if (res === 'eq') {
      return compare(tail(left), tail(right))
    }
    return res
  }
  if (Array.isArray(leftValue) & !Array.isArray(rightValue)) {
    const res = compare(leftValue, [rightValue])
    if (res === 'eq') {
      return compare(tail(left), tail(right))
    }
    return res
  }
  if (!Array.isArray(leftValue) & Array.isArray(rightValue)) {
    const res = compare([leftValue], rightValue)
    if (res === 'eq') {
      return compare(tail(left), tail(right))
    }
    return res
  }
}

assert(compare([], [[]]))
assert(compare([], [1]))
assert(compare(3, 5))
assert(compare([[1]], [[1]]))

const res = pairs.map((pair, idx) => {
  const [leftRaw, rightRaw] = pair.split('\n')
  const [left, right] = [leftRaw, rightRaw].map(JSON.parse)
  // console.log(left, '\n', right)
  
  return [idx, compare(left, right)]
})

console.log(sum(res.filter(r => r[1]).map(r => r[0] + 1)))
