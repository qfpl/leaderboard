{ oauthClientID, oauthClientSecret }:
let
  port = 8080;
  pgPort = 5432;
  pgUser = "test";
  pgDb = "leaderboard";
  pgPassword = "testPassword";j
  oauthAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth";
  oauthAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v3/token";
  oauthCallback = ;
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
          Environment = ''
            DB_PASS=${pgPassword}
            OAUTH_CLIENT_ID=${oauthClientID}
            OAUTH_CLIENT_SECRET=${oauthClientSecretID}
            OAUTH_AUTHORIZE_ENDPOINT=${oauthAuthorizeEndpoint}
            OAUTH_ACCESS_TOKEN_ENDPOINT=${oauthAccessTokenEndpoint}
            OAUTH_CALLBACK=${oauthCallback}
          '';
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
