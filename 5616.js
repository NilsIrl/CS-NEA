(()=>{var e,t,r,n,o,a,s={},i={};function p(e){var t=i[e];if(void 0!==t)return t.exports;var r=i[e]={id:e,loaded:!1,exports:{}};return s[e](r,r.exports,p),r.loaded=!0,r.exports}p.m=s,e="function"==typeof Symbol?Symbol("webpack then"):"__webpack_then__",t="function"==typeof Symbol?Symbol("webpack exports"):"__webpack_exports__",r="function"==typeof Symbol?Symbol("webpack error"):"__webpack_error__",n=e=>{e&&(e.forEach((e=>e.r--)),e.forEach((e=>e.r--?e.r++:e())))},o=e=>!--e.r&&e(),a=(e,t)=>e?e.push(t):o(t),p.a=(s,i,p)=>{var c,l,u,f=p&&[],b=s.exports,d=!0,h=!1,m=(t,r,n)=>{h||(h=!0,r.r+=t.length,t.map(((t,o)=>t[e](r,n))),h=!1)},y=new Promise(((e,t)=>{u=t,l=()=>(e(b),n(f),f=0)}));y[t]=b,y[e]=(e,t)=>{if(d)return o(e);c&&m(c,e,t),a(f,e),y.catch(t)},s.exports=y,i((s=>{var i;c=(s=>s.map((s=>{if(null!==s&&"object"==typeof s){if(s[e])return s;if(s.then){var i=[];s.then((e=>{p[t]=e,n(i),i=0}),(e=>{p[r]=e,n(i),i=0}));var p={};return p[e]=(e,t)=>(a(i,e),s.catch(t)),p}}var c={};return c[e]=e=>o(e),c[t]=s,c})))(s);var p=()=>c.map((e=>{if(e[r])throw e[r];return e[t]})),l=new Promise(((e,t)=>{(i=()=>e(p)).r=0,m(c,i,t)}));return i.r?l:p()}),(e=>(e&&u(y[r]=e),l()))),d=!1},p.d=(e,t)=>{for(var r in t)p.o(t,r)&&!p.o(e,r)&&Object.defineProperty(e,r,{enumerable:!0,get:t[r]})},p.f={},p.e=e=>Promise.all(Object.keys(p.f).reduce(((t,r)=>(p.f[r](e,t),t)),[])),p.u=e=>e+".js",p.g=function(){if("object"==typeof globalThis)return globalThis;try{return this||new Function("return this")()}catch(e){if("object"==typeof window)return window}}(),p.hmd=e=>((e=Object.create(e)).children||(e.children=[]),Object.defineProperty(e,"exports",{enumerable:!0,set:()=>{throw new Error("ES Modules may not assign module.exports or exports.*, Use ESM export syntax, instead: "+e.id)}}),e),p.o=(e,t)=>Object.prototype.hasOwnProperty.call(e,t),p.r=e=>{"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(e,"__esModule",{value:!0})},p.v=(e,t,r,n)=>{var o=fetch(p.p+""+r+".module.wasm");return"function"==typeof WebAssembly.instantiateStreaming?WebAssembly.instantiateStreaming(o,n).then((t=>Object.assign(e,t.instance.exports))):o.then((e=>e.arrayBuffer())).then((e=>WebAssembly.instantiate(e,n))).then((t=>Object.assign(e,t.instance.exports)))},(()=>{var e;p.g.importScripts&&(e=p.g.location+"");var t=p.g.document;if(!e&&t&&(t.currentScript&&(e=t.currentScript.src),!e)){var r=t.getElementsByTagName("script");r.length&&(e=r[r.length-1].src)}if(!e)throw new Error("Automatic publicPath is not supported in this browser");e=e.replace(/#.*$/,"").replace(/\?.*$/,"").replace(/\/[^\/]+$/,"/"),p.p=e})(),(()=>{var e={5616:1};p.f.i=(t,r)=>{e[t]||importScripts(p.p+p.u(t))};var t=self.webpackChunkprog_www=self.webpackChunkprog_www||[],r=t.push.bind(t);t.push=t=>{var[n,o,a]=t;for(var s in o)p.o(o,s)&&(p.m[s]=o[s]);for(a&&a(p);n.length;)e[n.pop()]=1;r(t)}})(),p.e(2221).then(p.bind(p,2221)).then((e=>{e.init(),self.addEventListener("message",(t=>{const r=[t.data.settings.caseSensitive,!t.data.settings.allowSingleQuotes,t.data.settings.allowWrongNext];switch(t.data.type){case"code":e.run(t.data.inner,...r);break;case"ast":e.ast(t.data.inner,...r)}self.postMessage({type:"done"})})),self.postMessage({type:"ready"})}))})();