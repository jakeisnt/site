import { Path } from "../../utils/path";

import type { PageSettings } from "../../types/site";
import { SourceFile } from ".";
import type { Dependency } from "../../types/html";

/**
 * Convert a file, wrapping that a file in a parent.
 * The parent file dispatches to a cloned child file for info.
 */
const wrapFile = (
  sourceFile: SourceFile, // the ts file
  getText: (source: SourceFile) => string,
  path: Path,
  {
    extension,
    mimeType,
    isDirectory = false,
  }: {
    extension: string;
    mimeType: string;
    isDirectory?: boolean;
  },
  getDependencies: (
    source: SourceFile,
    settings: PageSettings
  ) => File[] = () => []
) => {
  const wrappingFile = sourceFile.clone();

  wrappingFile.fakeFileOf = sourceFile;

  wrappingFile.write = (config: PageSettings) => {
    const { sourceDir, targetDir } = config;

    // Write the wrapping file
    const targetPath = sourceFile.path.relativeTo(sourceDir, targetDir);
    targetPath.writeString(sourceFile.text);

    // write the javascript file without an extension
    // const noExtensionPath = targetPath.replaceExtension();
    // noExtensionPath.writeString(sourceFile.text);

    // Write the source file
    sourceFile.write(config);

    return wrappingFile;
  };

  wrappingFile.dependencies = (settings: PageSettings) => {
    return getDependencies(sourceFile, settings);
  };

  Object.defineProperty(wrappingFile, "text", {
    get() {
      // There's an inneficiency. We compile on the fly every time.
      // This can be cached!
      return getText(sourceFile);
    },
  });

  Object.defineProperty(wrappingFile, "mimeType", {
    get() {
      return mimeType;
    },
  });

  // the path of this new source file needs to resolve to the old path
  Object.defineProperty(wrappingFile, "path", {
    get() {
      return path.replaceExtension(extension);
    },
  });

  Object.defineProperty(wrappingFile, "require", {
    get() {
      return require(path.toString());
    },
  });

  Object.defineProperty(wrappingFile, "isDirectory", {
    get() {
      return isDirectory;
    },
  });

  return wrappingFile; // the js file
};

export { wrapFile };
