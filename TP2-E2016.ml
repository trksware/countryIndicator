(*********************************************************************)
(* Langages de Programmation: IFT 3000 NRC 51158                     *)
(* TP2 ÉTÉ 2016. Date limite: Jeudi 14 juillet à 17h                 *)
(* Implanter un système d'indicateurs de développement               *)
(* en utilisant les données ouvertes de la banque mondiale           *)
(*********************************************************************)
(*********************************************************************)
(* Étudiant(e):                                                      *)
(* NOM: Ellouadghiri Elidrissi_ PRÉNOM: Tarek_______________________ *) 
(* MATRICULE: 111134746________ PROGRAMME: IFT______________________ *)
(*                                                                   *)
(*********************************************************************)

(* Chargement de modules, fonctions et librairies utiles pour le TP2 *)
#use "utiles.ml";;
 
(* Chargement de la signature du TP2 *)
#use "TP2-SIG-E2016.mli";;

(********************************************************************) 
(* Implantation du système en utilisant                             *)
(* la programmation orientée objet                       	    *) 
(********************************************************************)

(* Module du TP *)

module Tp2e16 : TP2E16 = struct 

  (* Classes du TP *)

  class indicateur (lch:string list) = 
    object(self)
      val nom_pays : string = nth lch 0
      val code_pays : string = nth lch 1
      val nom_indicateur : string = nth lch 2
      val valeurs_annees : (int * float) list = 
        if (length lch) < 4 then failwith "La longueur de la liste est incorrecte" 
        else let rl = tl(tl(tl(tl lch))) in
          mapi (fun i x -> if (x="") then (i+1960,-1.0) else (i+1960,float_of_string x)) rl
      val code_indicateur : string = nth lch 3

      method get_nom_pays = nom_pays
      method get_code_pays = code_pays
      method get_nom_indicateur = nom_indicateur
      method get_valeurs_annees = valeurs_annees
      method get_code_indicateur = code_indicateur

      (* Méthode à implanter *)
      
      (* afficher_indicateur : unit *)
      method afficher_indicateur = 
	print_string "Code de l'indicateur: ";
	print_string self#get_code_indicateur;
	print_string "\n";

	print_string "Nom de l'indicateur: ";
	print_string self#get_nom_indicateur;
	print_string "\n";

	print_string "Code du pays: ";
	print_string self#get_code_pays;
	print_string "\n";

	print_string "Nom du pays: ";
	print_string self#get_nom_pays;
	print_string "\n"

    end


  class sysindicateurs (od:string) (td:string) =
    object
	val origine_donnees : string = od 
	val theme_donnees : string = td 
	method get_origine_donnees = origine_donnees
	method get_theme_donnees = theme_donnees
    end


  class sysind_education (od:string) (td:string) =
    object(self)
      inherit sysindicateurs od td as parent
      val mutable map_indicateurs : indicateur IndicateursMap.t ref = ref (IndicateursMap.empty)
      method get_map_indicateurs = map_indicateurs
      method set_map_indicateurs (mi: indicateur IndicateursMap.t ref) = map_indicateurs <- mi

      method ajouter_indicateur (lch: string list) = 
        map_indicateurs := IndicateursMap.add (nth lch 1,List.nth lch 3) (new indicateur lch) !map_indicateurs

      method ajouter_liste_indicateurs (llch:string list list) =
        List.iter (fun lch -> (self#ajouter_indicateur lch)) llch

      method charger_donnees (fichier:string) =
        let ic =  try open_in fichier with _ -> failwith "Fichier inacessible" in
        let _ = input_line ic in (* ignorer les 5 premières lignes *)
        let _ = input_line ic in
        let _ = input_line ic in
        let _ = input_line ic in
        let _ = input_line ic in
        let liste_lignes = lire_fichier ic ";" in
        close_in ic; map_indicateurs := IndicateursMap.empty; self#ajouter_liste_indicateurs liste_lignes

      method retourner_donnees : indicateur list list =
        let li = IndicateursMap.bindings !map_indicateurs in
        let lp = uniques(map (fun (k,i) -> fst k) li) in
        List.map (fun p -> IndicateursMap.fold (fun _ i acc -> acc@[i]) 
           (IndicateursMap.filter (fun (x,y) ind -> x = p) !map_indicateurs) []) lp

      method indicateur_existe (ind:indicateur)  =
        IndicateursMap.exists (fun k d -> d = ind) !map_indicateurs

      method retourner_nbr_indicateurs =
        IndicateursMap.cardinal !map_indicateurs

      (* Méthodes à implanter *)

      (* retourner_indicateur : string * string -> indicateur *)
      method retourner_indicateur (cle:string*string) =
	IndicateursMap.find cle !map_indicateurs

      (* supprimer_indicateur : string * string -> unit *)
      method supprimer_indicateur (cle:string*string) =
	 map_indicateurs := IndicateursMap.remove cle !map_indicateurs

      (* supprimer_liste_indicateurs : (string * string) list -> unit *)
      method supprimer_liste_indicateurs (lcles:(string*string) list) =
	 List.iter self#supprimer_indicateur lcles

      (* afficher_indicateurspays : indicateur list -> unit *)
      method afficher_indicateurspays (ip:indicateur list) =
        List.iter (fun x -> x#afficher_indicateur) ip

      (* afficher_valeur_indicateur : indicateur -> int -> unit *)
      method afficher_valeur_indicateur (ind:indicateur)(annee:int)=
        let l = ind#get_valeurs_annees in
        let v = List.assoc annee l in
        if v = -1.0 then print_string "Donne manquante" else let w = string_of_float v in print_string w
        

      (* retourner_indicateurs_pays : string -> indicateur list *)
      method retourner_indicateurs_pays (cp:string): indicateur list =
	let li = self#retourner_donnees in
        let le = List.concat li in 
        List.filter (fun x -> x#get_code_pays = cp) le


      initializer print_string ("Recherche dans " ^ (parent#get_origine_donnees) ^ 
				" pour le theme de " ^ (self#get_theme_donnees) ^ ".");
				print_newline();
    end

  class app_sysindicateurs (nf:string) (i:bool) =
    object(self)
      val nom_fichier = nf
      val interface = i
      val mutable cpays = ""
      val mutable cind = 0
      method private set_cpays x = cpays <- x
      method private set_cind x = cind <- x

      method private retourner_chi (i:indicateur) =
        let s = "Code de l'indicateur: " ^ i#get_code_indicateur ^ ".\n" in
        let s = s ^ "Nom de l'indicateur: " ^ i#get_nom_indicateur ^ ".\n" in
        let s = s ^ "Code du pays: " ^ i#get_code_pays ^ ".\n" in
        let s = s ^ "Nom du pays: " ^ i#get_nom_pays ^ ".\n" in
        let s = s ^ "\nValeurs de l'indicateur selon les années:\n\n" in
        let vi = i#get_valeurs_annees in
        let c = fold_left (fun accu paire -> (string_of_int (fst paire)) ^ ": " 
                                             ^ (string_of_float (snd paire)) ^ "\n" ^ accu) "" vi in
        s ^ c
 
      (* Méthodes à implanter *)

      (* sauvegarder_indicateur : indicateur -> out_channel -> unit *)     
      method sauvegarder_indicateur (i:indicateur) (flux:out_channel) =
	let ch = self#retourner_chi i in output_string flux ch

      (* lancer_systeme_indicateurs : unit *)
      method lancer_systeme_indicateurs =
	print_string "Outil de recherche d'indicateurs";
	let si = new sysind_education "les donnees ouvertes de la banque mondiale" "l'education" in
	print_string "Chargement des donnees en cours ...\n";
	flush stdout;
        let _,t = timeRun si#charger_donnees nf in
	let a = string_of_float t in
	print_string "Chargement termine dans un temps de: ";
	print_string a;
        print_string "\n";
	print_string "Veuillez entrer le code du pays qui vous interesse: ";
        print_string "\n";
        let choix_pays = read_line () in
	let li_ind_pays = si#retourner_indicateurs_pays choix_pays in
        if length li_ind_pays <> 0 then
	let nb_ind_pays = length li_ind_pays in
	print_string "Nombre d'indicateurs trouves pour ce pays: ";
	print_int nb_ind_pays;
        print_string "\n";
	print_string "Veuillez entrer le numero de l'indicateur que vous voulez afficher (de 1 a ";
	print_int nb_ind_pays;
	print_string  ")";
        print_string "\nVeuillez svp apuyer sur shift+entr puis ;;";
        ignore(read_line());
        let choix_ind = read_int () in
        if (choix_ind > 0 && choix_ind < nb_ind_pays) then
        let b = choix_ind - 1 in
        let c = List.nth li_ind_pays b in
        let file = open_out "test.txt" in
        self#sauvegarder_indicateur c file;
        close_out file
        else print_string "Aucune donnee trouvee, veuillez verifier le code du pays.\n"
        else print_string "Aucune donnee trouvee, veuillez verifier le code du pays.\n"
	

      (* lancer_interface_sindicateurs : unit *)
      method lancer_interface_sindicateurs =
	(* À compléter *)
	let si = new sysind_education "les donnees ouvertes de la banque mondiale" "l'education" in
        print_string "Chargement des donnees en cours ...\n";
        flush stdout;
        let _,_ = timeRun si#charger_donnees nom_fichier in
       
        (* ... *)
        let top = openTk () in
	Wm.title_set top "Système d'indicateurs";
        Wm.geometry_set top "680x580";
        let l1 = Label.create ~text:"Bienvenue a l'outil de recherche d'indicateurs" top in
       (************************)

        let ma_frame_c = Frame.create top in 
        let l_pays = let li = si#retourner_donnees in List.map (fun x -> ((List.hd x)#get_code_pays) ^ " : " ^ ((List.hd x)#get_nom_pays)) li in
        let my_pays = Listbox.create ~selectmode: `Single ma_frame_c in
        let _ = Listbox.insert
          ~index: `End
          ~texts: l_pays
          my_pays in

        let l_ind = let li = si#retourner_donnees in
          let le = List.hd li in
          List.map (fun x -> (x#get_code_indicateur) ^ " : " ^ (x#get_nom_indicateur)) le in
        let my_ind = Listbox.create ~selectmode: `Single ma_frame_c in
        let _ = Listbox.insert
          ~index: `End
          ~texts: l_ind
          my_ind in


        (* ma_frame*)
        let ma_frame = Frame.create top in 
        let text = Text.create ~width:20 ~height:8 ma_frame in
        Text.insert ~index:(`End,[]) ~text:"Affichage de l'indicateur" text;
        let scroll = Scrollbar.create ma_frame in
        let scroll_link s tx =
        Text.configure ~yscrollcommand:(Scrollbar.set s) tx;
        Scrollbar.configure ~command:(Text.yview tx) s in
        scroll_link scroll text;
      
        pack ~side:`Right ~fill:`Y [scroll];
        pack ~side:`Left ~fill:`Both ~expand:true [text];

        let aff_pays =
          fun () ->
            if (length (Listbox.curselection my_pays) <> 0) then
            let n = match (List.hd (Listbox.curselection my_pays)) with
            |`Num x -> x
            | _ -> failwith "pas de selection"
            in
            let n1 = List.nth l_pays n in
            let n2 = Char.escaped (String.get n1 0) ^ Char.escaped(String.get n1 1) ^ Char.escaped(String.get n1 2) in
            self#set_cpays n2 in

           let aff_ind =
            fun () ->
              if (length (Listbox.curselection my_ind) <> 0) then
              let m = match (List.hd (Listbox.curselection my_ind)) with
              |`Num y -> y
              | _ -> failwith "pas de selection"
              in
              self#set_cind m
              in

           let trouve_id =  
            fun () ->
              if (cpays <> "" && cind <> 0) then
              let s = si#retourner_indicateurs_pays cpays in
              let t = List.nth s cind in
              let u =  self#retourner_chi t in
              Text.delete text ((`Linechar (0,0)),[]) (`End,[]);
              Text.insert ~index:(`End,[]) ~text:u text;
              else
                Text.delete text ((`Linechar (0,0)),[]) (`End,[]);
                Text.insert ~index:(`End,[]) ~text:"\nVeuillez entrer les bonnes selections" text;
              in

           let b_pays = Button.create ma_frame_c
            ~text:"Valider le choix du pays"
            ~background:`Red
            ~command:(aff_pays)
            in
   
           let b_ind = Button.create ma_frame_c
            ~text:"Valider le choix de l'indicateur"
            ~background:`Yellow
            ~command:(aff_ind)
            in

          let b_aff = Button.create ma_frame_c
            ~text:"Afficher les résultats"
            ~background:`Green
            ~command:(trouve_id)
            in


        pack [l1];

        pack [my_pays] ~side:`Top ~fill:`X;
        pack [b_pays] ~side:`Top ~fill:`X;

        pack [my_ind] ~side:`Top ~fill:`X;
        pack [b_ind] ~side:`Top ~fill:`X;

        pack [b_aff] ~side:`Top ~fill:`X;

        pack [ma_frame_c] ~side:`Left ~expand:true ~fill:`Both;
        pack [ma_frame] ~side:`Right ~expand:true ~fill:`Both;

        (* ... *)	     
        let _ = Printexc.print mainLoop () in
	print_endline "Merci et au revoir!\n"


      initializer if interface then self#lancer_interface_sindicateurs else self#lancer_systeme_indicateurs

    end

end
