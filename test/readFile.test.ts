import { expect, test } from "bun:test";
import { readFile } from "../src/file";
import { PAGE_SETTINGS } from "./testConstants";
import { Path } from "../src/utils/path";
import { File } from "../src/file/classes";

test("readFile works", () => {
  const file = readFile(
    Path.create("./test/readFile.test.ts.html"),
    PAGE_SETTINGS
  );
  expect(file).toBeTruthy();
  if (!file) return;

  const fileDirectory = readFile(Path.create("./test/"), PAGE_SETTINGS);
  expect(fileDirectory).toBeTruthy();
  if (!fileDirectory) return;

  expect(file?.directory() as File).toEqual(fileDirectory);
});
