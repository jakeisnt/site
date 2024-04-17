import type { Path } from "../utils/path";

type PageSettings = {
  siteName: string;
  sourceDir: Path;
  targetDir: Path;
  resourcesDir: Path;
  faviconsDir: Path;
  fallbackSourceDir?: Path;
  ignorePaths?: string[];

  // likely only used for serving, maybe not for local dev
  url: string;
  host: string;
  port: number;
  websocketPath: string;
};

export type { PageSettings };
