import fs from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";

const [webPath, wasmPath, hostPath] = process.argv.slice(2);
if (!webPath || !wasmPath || !hostPath) {
  throw new Error("usage: verify-kotoba.mjs WEB.mjs MODULE.wasm BROWSER-HOST.mjs");
}

const web = await import(pathToFileURL(path.resolve(webPath)));
if (web.kotobaArtifact.requiredCapabilities.length !== 0) {
  throw new Error("canonical document Web artifact requested a capability");
}
if (web.instantiateKotoba().main() !== 42n) {
  throw new Error("canonical document Web self-check mismatch");
}

const host = await import(pathToFileURL(path.resolve(hostPath)));
const wasm = await host.instantiateKotoba(fs.readFileSync(path.resolve(wasmPath)));
if (wasm.instance.exports.main() !== 42n) {
  throw new Error("canonical document Wasm self-check mismatch");
}

console.log("edn: canonical document Web/Wasm conformance passed");
