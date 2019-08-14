function mkcd
	 mkdir -p $argv[1] && cd $argv[1];
	 ln -s /mnt/Long/Claude-Alban/Documents/LaTeX/Professionnel/Modèles/Lettre\ de\ motivation/ template; 
	 ln -s /mnt/Long/Claude-Alban/Documents/LaTeX/Professionnel/Modèles/CV/ cv-template; 
	 ln -s /mnt/Long/Claude-Alban/Documents/LaTeX/texmf/tex/latex/misc/fonts/ fonts; 
	 cp /mnt/Long/Claude-Alban/Documents/LaTeX/Professionnel/Modèles/CV/cv.md cv.md
	 cp /mnt/Long/Claude-Alban/Documents/LaTeX/Professionnel/Modèles/Lettre\ de\ motivation/lettre.md $argv[1].md
	 nvim cv.md $argv[1].md
 end
