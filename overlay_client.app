{application, overlay_client,
	[{description, "The overlay file sharing client" },
	 {vsn, "1.0" },
	 {modules, [overlay_client_app, overlay_client]},
	 {registered,[]},
	 {applications, [kernel,stdlib]},
	 {mod, {overlay_client_app,[{s1,["c1","c2","c3","c4","c5","c6","c7","c8","c9","c10"]},
								{s2,["d1","d2","d3","d4","d5"]}]}}
    ]}.