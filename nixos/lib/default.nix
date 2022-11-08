{ inputs, lib, ... }:

rec {
  listFolders = (dir:
    lib.mapAttrs (n: v: dir + "/${n}")
    (lib.filterAttrs (n: t: t == "directory" || t == "symlink")
      (builtins.readDir dir)));
  systemConfigs = (dir:
    lib.mapAttrs
    (host: path: (import path { inherit inputs lib; }).systemConfig)
    (listFolders dir));
}
