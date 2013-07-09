// environment=underscore

var cloneObjOrig = {a: 1}, copyObjCopy = _.clone(copyObjOrig);
cloneObjOrig.a; //: number
cloneObjCopy.a; /*failing*///: number
cloneObjCopy.b = true;
cloneObjOrig.b; //: ?

var extendObj = {a: 1, b: true};
_.extend(extendObj, {c: ''}, {d: 2});
extendObj; //:: {a: number, b: bool, c: string, d: number}
