// plugin=node
// plugin=es_modules

import foo from "./foo"
foo //: number
import {hello as holle} from "./foo"
holle //: fn() -> bool

import * as blah from "./blah"
blah //:: {a: number, b: string, c: bool, d: fn() -> number}

import * as reexp from "./reexp"
reexp //:: {a: number, b: bool}
