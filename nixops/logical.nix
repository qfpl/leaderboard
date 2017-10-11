{
  network.description = "leaderboard";
  application =
    { config, pkgs, ... }:
    let
      leaderboard = import ../default.nix { nixpkgs = pkgs; };
    in
    {
      environment.systemPackages = [ leaderboard ];
      networking.firewall.allowedTCPPorts = [ 8080 ];

      systemd.services.leaderboard = {
        description = "leaderboard";
        after = [ "network.target" ];
        script = "${leaderboard}/bin/leaderboard";
        serviceConfig = {
          Restart = "always";
        };
      };
    };

  database =
    { config, pkgs, ... }:
    {
      services.postgresql.enable = true;
      services.postgresql.package = pkgs.postgresql94;
    };
}
