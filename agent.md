# Agent and contributor guidance

Read `context.md` before changing pipeline behavior.

1. Preserve the public contract: polygon/multipolygon in, metrics plus
   provenance out.
2. Keep source selection geometry-based and auditable. Never use a district
   bounding box as the final source assignment.
3. Never infer QL from point density. Metadata uncertainty must be surfaced as
   a preflight result or explicit override.
4. Do not silently skip uncovered, failed, or unsupported source areas.
5. Keep raw EPT extracts transient; retain normalized halo blocks by default.
6. Maintain resume safety and do not overwrite completed outputs without an
   explicit user choice.
7. Test a small AOI before changing the production path. Preserve existing
   metric validation fixtures and native-CRS computation rules.
8. Treat external catalog/index services as replaceable adapters. Cache their
   resolved metadata and provide a fallback path where practical.

When adding an option, decide whether it belongs to the public package API or
is an implementation detail. Prefer a stable high-level API.
