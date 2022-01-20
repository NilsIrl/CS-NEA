import { run } from "prog-wasm";

const form = document.getElementById("code-form");
const textarea = <HTMLTextAreaElement>document.getElementById("code-textarea");
const output = <HTMLOutputElement>document.getElementById("output");

function clear_output() {
  output.value = "";
}

function append_to_output(text: string) {
    output.value += text;
}

form.addEventListener("submit", e => {
    e.preventDefault();
    run(textarea.value);
})
