import { run } from "prog-wasm";

const form = document.getElementById("code-form");
const textarea = <HTMLTextAreaElement>document.getElementById("code-textarea");

form.addEventListener("submit", e => {
    e.preventDefault();
    run(textarea.value);
})
