import { Path } from "utils/path";
import { expect, test } from "bun:test";

test("Create a normalized absolute path", () => {
  expect(Path.create("/")).toEqual(new Path("/"));
  expect(Path.create("/a/b/c")).toEqual(Path.create("/a/b/c/"));
});

test("Create a relative path", () => {
  expect(Path.create("./test")).toEqual(Path.create(process.cwd() + "/test"));
});

test("Get the parent of a path", () => {
  expect(Path.create("/a/b/c").parent).toEqual(Path.create("/a/b"));
  expect(Path.create("/a/b/c/").parent).toEqual(Path.create("/a/b"));
  expect(Path.create("/").parent).toEqual(Path.create("/"));
});

test("relativeTo paths", () => {
  const path = Path.create("/a/b/c");
  const relative = path.relativeTo("/a");
  expect(relative).toEqual(new Path("/b/c"));
});

test("relativeTo both args", () => {
  const path = Path.create(
    "/Users/jake/Desktop/personal/site/resources/style.css"
  );

  console.log("argumetns", {
    maybeOtherPath: "/Users/jake/Desktop",
    maybeReplaceWithPath: "/Users/jake/wiki",
  });

  const relative = path.relativeTo("/Users/jake/Desktop", "/Users/jake/wiki");
  expect(relative).toEqual(
    new Path("/Users/jake/wiki/site/resources/style.css")
  );
});

test("extension", () => {
  expect(Path.create("/a/b/c").extension).toEqual(null);
  expect(Path.create("/a/b/c.md").extension).toEqual("md");
  expect(Path.create("/a/b/c.md").extension).toEqual("md");

  // test the extensions of directories
  expect(Path.create("/a/b/c").extension).toEqual(null);
  expect(Path.create("/a/b/c/").extension).toEqual(null);
});

test("isRootPath", () => {
  expect(Path.create("/").isRootPath()).toEqual(true);
  expect(Path.create("").isRootPath()).toEqual(false);
  expect(Path.create("/a").isRootPath()).toEqual(false);
  expect(Path.create("/a/b/c").isRootPath()).toEqual(false);
});
