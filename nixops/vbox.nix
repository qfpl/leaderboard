{
  application =
    { config, pkgs, ... }:
    {
      deployment = {
        targetEnv = "virtualbox";
        virtualbox = {
          memorySize = 512;
          vcpu = 2;
          headless = true;
        };
      };
    };

  database =
    { config, pkgs, ... }:
    {
      deployment = {
        targetEnv = "virtualbox";
        virtualbox = {
          memorySize = 512;
          vcpu = 2;
          headless = true;
        };
      };
    };
}
