// plugin=node
// plugin=es_modules

import foo from "./foo"
foo //: number
import {hello as holle //<loc: 3, 16
        he //+ hello, heythere
        } from "./foo"
holle //: fn() -> bool

import {//+ isatty
       } from "tty"

import * as blah //<loc: 1, 0
       from "./blah"
blah //:: {a: number, b: string, c: bool, d: fn() -> number}

import * as reexp from "./reexp"
reexp //:: {a: number, b: bool}

import "./b" //+ "./blah"

import C from "./class"

(new C()). //+ methodA

import f from "./func"

f //: fn() -> bool

import o from "./obj"

o.propA //: number
o.propB //: string
