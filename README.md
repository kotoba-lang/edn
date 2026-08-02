# kotoba-lang/edn

Bounded EDN and canonical structured-document codecs for Kotoba applications.

## Surfaces

- `kotoba.lang.edn` (`.cljc`) provides single-form `read-string` and
  `write-string` for immediate replacement of direct `clojure.edn` and
  `cljs.reader` dependencies. Tagged literals, reader discard, and reader eval
  are forbidden. Bytes, depth, tokens, nodes, and strings are bounded.
- `kotoba.lang.canonical-document` (`.kotoba`) provides the sovereign,
  zero-capability codec for the compiler's bounded `:document` value.

The compiler's current `document-read` / `document-print` format is canonical
hex, not textual EDN. Textual EDN becomes Kotoba-source authority only after
the compiler admits `document-edn-read` / `document-edn-print`; the boundary is
recorded in `migration/bounded-edn-v1.edn`.

## Use

```clojure
(require '[kotoba.lang.edn :as edn])

(edn/read-string "{:goal \"migrate\"}")
(edn/write-string {:goal "migrate"})
```

## Verify

```sh
clojure -M:test
npm install
npm test
clojure -M:lint
```
