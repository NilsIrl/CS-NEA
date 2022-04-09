import "xterm/css/xterm.css";
import * as monaco from 'monaco-editor/esm/vs/editor/editor.api';

import { Terminal } from "xterm";
import { FitAddon } from "xterm-addon-fit";

import { Sender } from "../../prog-wasm-channel/pkg";

const editor = monaco.editor.create(document.getElementById("code-monaco"), {
  value: 'print("Hello World!")',
});

const term = new Terminal({ convertEol: true });

const runButton = document.getElementById("run-button");
const printAstButton = document.getElementById("print-ast");

const timeCheckbox = document.getElementById("time-taken");
const caseSensitiveCheckbox = document.getElementById("case-sensitive");
const allowSingleQuotesCheckbox = document.getElementById("allow-single-quotes");
const allowWrongNextCheckbox = document.getElementById("allow-wrong-next");

function workerListener(e) {
  switch (e.data.type) {
    case "print":
      term.write(e.data.inner);
      break;
    case "done":
      runButton.innerText = "Run!";
      if (timeCheckbox.checked) {
        const timeElapsed = (performance.now() - start_time) / 1000;
        term.write(`${timeElapsed} seconds elapsed.\n`)
      }
      break;
    case "ready":
      runButton.disabled = false;
      printAstButton.disabled = false;
      break;
  }
}

function workerErrorListener(e) {
  terminate_worker();
}

function start_worker() {
  worker = new Worker(new URL('worker.js', import.meta.url));
  worker.addEventListener("message", workerListener);
  worker.addEventListener("error", workerErrorListener);
  runButton.disabled = true;
  printAstButton.disabled = true;
}

function terminate_worker() {
  worker.terminate();
  runButton.innerText = "Run!";
  start_worker();
}

let worker;
let current_line = "";
let channel;
let start_time;
start_worker();

const fit_addon = new FitAddon();

term.loadAddon(fit_addon);
term.open(document.getElementById("terminal"));
fit_addon.fit();

term.onData((e) => {
  switch (e) {
    case "\u0003": // Ctrl + C
      term.write("^C");
      terminate_worker();
      break;
    case "\u007F": // Backspace
      if (current_line.length > 0) {
        term.write("\b \b");
        current_line = current_line.slice(0, -1);
      }
      break;
    case '\r':
      term.write("\n");
      for (const c of current_line) {
        channel.send(c);
      }
      channel.send("\n");
      current_line = "";
      break;
    default:
      current_line += e;
      term.write(e);
      break;
  }
});

function settings_dictionary() {
  return {
    caseSensitive: caseSensitiveCheckbox.checked,
    allowSingleQuotes: allowSingleQuotesCheckbox.checked,
    allowWrongNext: allowWrongNextCheckbox.checked,
  };
}

printAstButton.addEventListener("click", e => {
  term.clear();
  start_time = performance.now();
  worker.postMessage({
    type: "ast",
    inner: editor.getValue(),
    settings: settings_dictionary(),
  });
});

runButton.addEventListener("click", e => {
  if (runButton.innerText === "Stop") {
    terminate_worker();
  } else {
    term.clear();
    start_time = performance.now();
    channel = new Sender();
    current_line = "";
    worker.postMessage({
      type: "code",
      inner: editor.getValue(),
      channel: channel.replica(),
      settings: settings_dictionary(),
    });
    runButton.innerText = "Stop";
  }
});
