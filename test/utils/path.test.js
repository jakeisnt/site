import { Path } from "utils/path";
import { expect, test } from "bun:test";

test("Create a normalized path", () => {
  expect(Path.create("/")).toEqual(new Path("/"));
  expect(Path.create("/a/b/c")).toEqual(Path.create("/a/b/c/"));
});

test("relativeTo", () => {
  const path = Path.create("/a/b/c");
  const relative = path.relativeTo("/a");
  expect(relative).toEqual(new Path("/b/c"));
});
