import "xterm/css/xterm.css";
import { Terminal } from "xterm";

let term = new Terminal({
    convertEol: true,
    logLevel: "debug",
});

term.open(document.getElementById("terminal"));

const form = document.getElementById("code-form");
const textarea = <HTMLTextAreaElement>document.getElementById("code-textarea");

form.addEventListener("submit", e => {
    e.preventDefault();
    term.clear();
    const scriptRunner = new Worker(
        new URL('worker.ts', import.meta.url),
    );
    scriptRunner.onmessage = function(e) {
        console.log(e);
        term.write(e.data);
    }
    scriptRunner.postMessage(textarea.value);
    console.log("post message");
})
