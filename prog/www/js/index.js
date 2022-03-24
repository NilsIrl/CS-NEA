import "xterm/css/xterm.css";
import * as monaco from "monaco-editor";
import { Terminal } from "xterm";
import { FitAddon } from "xterm-addon-fit";

const editor = monaco.editor.create(document.getElementById("code-monaco"), {
    value: 'print("Hello World!")',
});

const term = new Terminal({ convertEol: true });

function workerListener(e) {
  switch (e.data.type) {
    case "print":
      term.write(e.data.inner);
      break;
    case "close":
      runButton.innerText = "Run!";
      break;

  }
}

function start_worker() {
  worker = new Worker(new URL('worker.js', import.meta.url));
  worker.addEventListener("message", workerListener);
}

function terminate_worker() {
  worker.terminate();
  runButton.innerText = "Run!";
  start_worker();
}

let worker;
start_worker();

const fit_addon = new FitAddon();

term.loadAddon(fit_addon);
term.open(document.getElementById("terminal"));
fit_addon.fit();

term.onKey((e) => {
    const ev = e.domEvent;
    if (ev.ctrlKey && ev.key == "c") {
        terminate_worker();
    }
});

const runButton = document.getElementById("run-button");
const printAstButton = document.getElementById("print-ast");

const textarea = document.getElementById("code-textarea");

printAstButton.addEventListener("click", e => {
  term.clear();
  worker.postMessage({
    type: "ast",
    inner: editor.getValue(),
  });
});

runButton.addEventListener("click", e => {
    if (runButton.innerText === "Stop") {
      terminate_worker();
    } else {
      term.clear();
      worker.postMessage({
        type: "code",
        inner: editor.getValue(),
      });
      runButton.innerText = "Stop";
    }
});
