import type { Path } from "../utils/path";

type PageSettings = {
  rootUrl: string;
  siteName: string;
  sourceDir: Path;
  targetDir: Path;
  resourcesDir: Path;
  faviconsDir: Path;
  fallbackSourceDir?: Path;
  ignorePaths?: string[];
};

export type { PageSettings };
