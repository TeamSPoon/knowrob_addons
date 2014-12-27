
/** <module> rosprolog - For when PrologMUD starts KnowRob (instead of the other way around)


@license LGPL
*/

:- set_prolog_flag(file_loading,verbose).
:- ensure_loaded('../../../../../src_lib/logicmoo_util/logicmoo_util_all.pl').

:- abolish(dbase_rosprolog,1), dynamic(dbase_rosprolog/1),prolog_load_context(file, Here),asserta(dbase_rosprolog(Here)).
dbase_rosprolog_dir(DIR):- dbase_rosprolog(Here),absolute_file_name(Here,AHere),file_directory_name(AHere,DIR).

user:file_search_path(knowrob,Where):- dbase_rosprolog_dir(DIR),concat_paths(DIR,'../../../*/',OOO),enumerate_files(OOO,Where),exists_directory(Where).
user:file_search_path(pack, knowrob).
user:file_search_path(ros, knowrob).



rdf_reload_file(File):-file_name_extension(_,'n3',File),!,rdf_unload(File),rdf_load(File,[format(trig),register_namespaces(true)]).
rdf_reload_file(File):-rdf_unload(File),rdf_load(File,[register_namespaces(true)]).

sw_file(Package,Subdir,File):- rospack_package_path(Package,PackageDir),
   concat_paths(PackageDir,Subdir,MiloDir),
   member(S,['./','./*/','./*/*/','./*/*/*','./*/*/*/*','./*/*/*/*/*']),
   concat_paths(MiloDir,S,FDir),
   concat_paths(FDir,'*{.n3,.owl,.ttl}',File).


ros_library_directory(WhereF2):-user:file_search_path(knowrob,W),once((atom_concat(W,'/*/prolog/',O),
  expand_file_name(O,ListE))),member(Where,ListE),normalize_path(Where,WhereF2).


% ros_package(B):-ros_library_directory(WhereF2),concat_paths(WhereF2,(..),O),file_base_name(O,B).
ros_package(RosPackName):-user:file_search_path(knowrob,W),once((atom_concat(W,'/*/',O),
  expand_file_name(O,ListE))),member(Where,ListE),normalize_path(Where,WhereF2),exists_directory(WhereF2),file_base_name(WhereF2,RosPackName).
  


rospack_initfile_path(RosPack,InitFile):-  
  rospack_package_path(RosPack, PackagePath), 
  concat_paths([PackagePath,'./prolog/init{.pl,.pl.in,}'],Where),expand_file_name(Where,[InitFile|_]),!.

%% rospack_package_path(+Package, -Path) is nondet.
%
% Locate ROS packages on the harddisk using 'rospack find'
% 
% @param Package  Name of a ROS package
% @param Path     Global path of Package determined by 'rospack find'
% 
rospack_package_path(Package, Path) :- var(Package),!,ros_package(Package),rospack_package_path(Package, Path).
rospack_package_path(Package, Path) :- is_absolute_file_name(Package),!,Path=Package.
rospack_package_path(Package, Path) :- expand_file_search_path(knowrob(Package),Path),exists_directory(Path),!.
rospack_package_path(Package, Path) :- trace,expand_file_search_path(knowrob(Package),Path),exists_directory(Path),!.
rospack_package_path(Package, Path) :- must(rospack_find_package_path(Package, Path)).

rospack_find_package_path(Package, Path) :- 
  nonvar(Package),
  process_create(path('rospack'), ['find', Package], [stdout(pipe(RospackOutput)), process(PID)]),
  read_line_to_codes(RospackOutput, C),
  string_to_list(Path, C),
  process_wait(PID, _).


%% init_ros_package(+PackagePath) is nondet.
%
% Initialize a KnowRob package by consulting the prolog/init.pl file
% 
% @param PackagePath  Path towards the package to be initialized
% 
init_ros_package( Package ) :- do_not_consult_ros_package( Package )  ,!.
init_ros_package( Package ) :- 
  rospack_package_path(Package, PackagePath),
  rospack_initfile_path(PackagePath,InitFile),
  exists_file(InitFile),
  set_prolog_flag(file_loading,verbose),
  must(consult(InitFile)), !.

init_ros_package( Package ):- dmsg(no_initfile(init_ros_package(Package))).

%% register_ros_package(+Package) is nondet.
%% register_ros_package(+Package, ?AbsoluteDirectory) is nondet.
%
% Find and initialize a KnowRob package, i.e. locate the package on the
% harddisk, add the path to the library search path, and consult the init.pl
% file that (recursively) initializes the package and its dependencies.
% 
% @param Package            Name of a ROS package
% @param AbsoluteDirectory  Global path to the 'prolog' subdirectory in the given package
% 
:- dynamic library_directory_addon/1.
:- assertz((user:library_directory(AddOnDir):-ros_library_directory(AddOnDir))).
:- assertz((user:library_directory(AbsoluteDirectory):- library_directory_addon(AbsoluteDirectory))).

:- dynamic(ros_package_initialized/1).

do_not_consult_ros_package( rosprolog ) :-!.
% do_not_consult_ros_package( jpl ) :-!.
% do_not_consult_ros_package( thea ) :-!.

register_ros_package( Package, _ ) :- do_not_consult_ros_package( Package )  ,!.
register_ros_package(Package, _) :-
  current_predicate(ros_package_initialized/1),
  ros_package_initialized(Package), !.
register_ros_package(Package, AbsoluteDirectory) :- 
  must(register_ros_package_now(Package, AbsoluteDirectory)).

register_ros_package_now(Package, AbsoluteDirectory) :-
  must(rospack_package_path(Package, PackagePath)),
  nonvar(PackagePath),
  atom_concat(PackagePath, '/prolog/', AbsoluteDirectory),
  retractall(library_directory_addon(AbsoluteDirectory)),
  asserta(library_directory_addon(AbsoluteDirectory)),
  assert(user:file_search_path(ros, AbsoluteDirectory)),
  catch((
  assert( ros_package_initialized(Package) ),
  add_ros_package_to_classpath(Package),
  init_ros_package( Package )),
      Ex,
      (retract( ros_package_initialized(Package) ), 
       throw(Ex))),!.

register_ros_package(Package) :-
  register_ros_package(Package, _).


use_ros_module(Package, FilePath) :-
  register_ros_package(Package, AbsoluteDirectory),
  atom_concat(AbsoluteDirectory, FilePath, AbsoluteFilePath),
  use_module( AbsoluteFilePath ).



%% add_ros_package_to_classpath(+Package) is nondet.
% 
% Adds Java dependencies of Package to the CLASSPATH environment variable
%
% @param Package Name of a ROS package
% 
add_ros_package_to_classpath(Package):-
	rospack_package_classpath(Package, PackagePath),	
	concat_env("CLASSPATH",PackagePath).

%% rospack_package_classpath(+Package, -Path) is nondet.
% 
% Calculates the Java dependencies of Package and returns a string to be appended to the CLASSPATH
%
% @param Package  Name of a ROS package
% @param Path     String with the dependencies to be added to the CLASSPATH
% 
% calculates java dependencies for classpath
rospack_package_classpath(Package, Path) :- rospack_package_path(Package, PackPath),atom_concat(PackPath,'**/*.jar',O),!,
  expand_file_name(O,ListE),atomic_list_concat(ListE,':',Path).
rospack_package_classpath(Package, Path) :- trace,rospack_package_path(Package, PackPath),atom_concat(PackPath,'**/*.jar',O),!,
  expand_file_name(O,ListE),atomic_list_concat(ListE,':',Path).
rospack_package_classpath(Package, Path) :-
  nonvar(Package),
  process_create(path('rosrun'), ['rosprolog', 'get_pkg_classpath', Package], [stdout(pipe(RospackOutput)), process(PID)]),
  read_line_to_codes(RospackOutput, C),
  string_to_list(Path, C),
  process_wait(PID, _).


prepend_atom_concat(Prefix,Base,Whole):-atom_concat(Prefix,_,Base),!,Whole=Base.
prepend_atom_concat(OldVal,Suffix,NewVal):-atom_concat(_,Suffix,OldVal),!,NewVal=OldVal.
prepend_atom_concat(OldVal,Suffix,NewVal):-atom_concat(OldVal,Suffix,NewVal),!.

% concat a value to an environment varible
% please note: delimiters have to be set within Val, e.g.:
% '/path/to/lib:/path/to/lib2'
concat_env(Var,Val):-
	(getenv(Var,OldVal)
	->  ((prepend_atom_concat(':',Val,PackagePath),prepend_atom_concat(OldVal,PackagePath,NewVal))) ;
        (NewVal = Val)),
	setenv(Var,NewVal).

forall_show(Call):- forall(Call,dmsg(Call)) *-> true; fmt(call_failed(Call)).


end_of_file.

:- register_ros_package(knowrob_common).
:- register_ros_package(knowrob_roslog_launch).
:- register_ros_package(knowrob_cram).
:- register_ros_package(knowrob_sim).
:- register_ros_package(knowrob_mongo).
:- register_ros_package(knowrob_vis).
:- register_ros_package(iai_semantic_maps).
:- register_ros_package(knowrob_srdl).
:- init_ros_package(knowrob_common).
% :- init_ros_package(prolog_perception).
:- init_ros_package(knowrob_actions).
% :- init_ros_package(ias_knowledge_base).
:- init_ros_package(comp_missingobj).
% :- init_ros_package(comp_semantic_map).
:- forall_show(user:file_search_path(knowrob,_Where)).

% :- forall(no_repeats(ros_package(B)),must(init_ros_package(B))).

:- forall_show(ros_package_initialized(_Where)).


% :- init_ros_package(semweb).









