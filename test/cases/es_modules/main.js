// plugin=node
// plugin=es_modules

import foo from "./foo"
foo //: number
import {hello as holle //<loc: 3, 16
       } from "./foo"
holle //: fn() -> bool

import * as blah //<loc: 1, 0
       from "./blah"
blah //:: {a: number, b: string, c: bool, d: fn() -> number}

import * as reexp from "./reexp"
reexp //:: {a: number, b: bool}

import "./b" //+ "./blah"
