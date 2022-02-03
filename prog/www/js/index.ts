import "xterm/css/xterm.css";
import * as monaco from "monaco-editor";
import { Terminal } from "xterm";
import { FitAddon } from "xterm-addon-fit";

const editor = monaco.editor.create(document.getElementById("code-monaco"), {
    value: 'print("Hello World!")',
});

let scriptRunner;

const term = new Terminal({
    convertEol: true,
    logLevel: "debug",
});
const fit_addon = new FitAddon();

term.loadAddon(fit_addon);
term.open(document.getElementById("terminal"));
fit_addon.fit();

function terminate_worker() {
    scriptRunner.terminate();
    runButton.innerText = "Run!";
}

term.onKey((e: {key: string, domEvent: KeyboardEvent }) => {
    const ev = e.domEvent;
    if (ev.ctrlKey && ev.key == "c") {
        terminate_worker();
    }
});

const runButton = document.getElementById("run-button");
const textarea = <HTMLTextAreaElement>document.getElementById("code-textarea");

runButton.addEventListener("click", e => {
    if (runButton.innerText === "Stop") {
        terminate_worker();
        return;
    }
    //e.preventDefault();
    term.clear();
    scriptRunner = new Worker(
        new URL('worker.ts', import.meta.url),
    );
    scriptRunner.addEventListener("message", function(e) {
        switch (e.data.type) {
            case "close":
                terminate_worker();
                break;
            case "print":
                term.write(e.data.inner);
                break;
        }
    });
    scriptRunner.postMessage({
        type: "code",
        inner: editor.getValue(),
    });
    runButton.innerText = "Stop";
    console.log("post message");
})
