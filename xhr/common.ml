let template_cache = CamlTemplate.Cache.create ~loader:(CamlTemplate.Cache.make_file_loader ~template_dir:"/") ()


