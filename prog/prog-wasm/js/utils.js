export function print(content) {
  self.postMessage({ type: "print", inner: content.slice() });
}