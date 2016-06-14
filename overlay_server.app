{application, overlay_server,
	[{description, "The overlay file sharing server" },
	 {vsn, "1.0" },
	 {modules, [overlay_server_app, overlay_supervisor, overlay_server]},
	 {registered,[overlay_server_app]},
	 {applications, [kernel,stdlib]},
	 {mod, {overlay_server_app,[s1,s2,s3]}}
    ]}.