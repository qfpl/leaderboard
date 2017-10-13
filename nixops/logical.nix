let
  port = 8080;
  pgPort = 5432;
  pgUser = "test";
  pgDb = "leaderboard";
  pgPassword = "testPassword";
  initScript = ''
    create role ${pgUser} password '${pgPassword}' createdb nocreaterole nosuperuser login;
    create database ${pgDb};
  '';
in
{
  network.description = "leaderboard";
  application =
    { config, pkgs, ... }:
    let
      leaderboard = import ../default.nix { nixpkgs = pkgs; };
    in
    {
      environment.systemPackages = [ leaderboard ];
      networking.firewall.allowedTCPPorts = [ 80 ];

      services.nginx = {
        enable = true;
        virtualHosts."${config.networking.privateIPv4}" = {
          locations."/" = {
            proxyPass = "http://0.0.0.0:${toString port}";
          };
        };
      };

      systemd.services.leaderboard = {
        description = "leaderboard";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Restart = "always";
          ExecStart = ''
            ${leaderboard}/bin/leaderboard \
              --db_host database \
              --db_user ${pgUser} \
              --db_port ${toString pgPort} \
              --db_name ${pgDb} \
              --port ${toString port} \
          '';
          Environment = "DB_PASS=${pgPassword}";
          StandardOutput = "syslog";
          StandardError = "syslog";
        };
      };
    };

  database =
    { config, pkgs, nodes, ... }:
    {
      networking.firewall.allowedTCPPorts = [ pgPort ];
      services.postgresql = {
        enable = true;
        authentication = ''
          host ${pgDb} ${pgUser} ${nodes.application.config.networking.privateIPv4}/32 trust
          host ${pgDb} ${pgUser} ${nodes.application.config.networking.privateIPv4}/32 md5
          host all all 0.0.0.0/0 md5
        '';
        package = pkgs.postgresql94;
        port = pgPort;
        enableTCPIP = true;
        initialScript = pkgs.writeText "init.sql" initScript;
      };
    };
}
