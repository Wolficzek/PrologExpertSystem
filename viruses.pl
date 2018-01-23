%Author Mateusz Dunaj

:-dynamic kind/1.
:-dynamic viruses/1.
:-dynamic current_virus/1.

define(known, 4):- dynamic(known/4).

%____RULES AND KNOWLEDGE BASE SECTION____

kind(X):-
    X == ransomware -> write(trojan), !;
    X == spyware -> write(trojan), !;
    X == adware -> write(trojan), !;
    X == botnet -> write(worm), !;
    X == rootkit -> write(worm), !;
    X == miner -> write(worm), !.

kind(_):- 
    write(unknown).

virus(ransomware):- 
    assert(current_virus(ransomware)),
    symptom(single_machine_infected),
    symptom(deleted_files),
    symptom(ransom),
    symptom(disk_encryption),
    source(email);
    source(social_engineering);
    source(suspicious_file).
     
virus(spyware):- 
    retractall(current_virus(_)),
    assert(current_virus(spyware)),
    symptom(network_traffic),
    symptom(multiple_account_breach),
    symptom(single_machine_infected),
    symptom(internet_connection_problems),
    source(email);
    source(social_engineering);
    source(suspicious_file);
    source(suspicious_ad_sites).
                        

virus(adware):- 
    retractall(current_virus(_)),
    assert(current_virus(adware)),
    symptom(network_traffic),
    symptom(high_CPU_usage),
    symptom(single_machine_infected),
    symptom(popup_ads),
    symptom(site_redirection),
    source(toolbar_addons);
    source(suspicious_ad_sites).


virus(botnet):- 
    retractall(current_virus(_)),
    assert(current_virus(botnet)),
    symptom(network_traffic),
    symptom(high_CPU_usage),
    symptom(multiple_machine_infected),
    source(suspicious_USB);
    source(unknown).

virus(rootkit):- 
    retractall(current_virus(_)),
    assert(current_virus(rootkit)),
    symptom(root_lock),
    symptom(permission_violation),
    symptom(multiple_machine_infected),
    source(suspicious_USB);
    source(connection_with_infected_machines).                        

virus(miner):- 
    retractall(current_virus(_)),
    assert(current_virus(miner)),
    symptom(network_traffic),
    symptom(high_CPU_usage),
    symptom(unusable_machine),
    symptom(multiple_machine_infected),
    source(email);
    source(suspicious_ad_sites); 
    source(unknown).

symptom(X):- ask('Do you have problem with symptom' ,X).
source(X):- ask('Can you define source of malicious software', X).

%_____ASK SECTION_____

%Dont ask if we know that its true

ask(Attribute,Value):-
    current_virus(X),
    known(yes,Attribute,Value,_),     
    retractall(known(yes,Attribute,Value,_)),
    assert(known(yes,Attribute,Value,X)), !.

%Dont ask if we know that its false

ask(Attribute,Value):-
  current_virus(X),
  known(no,Attribute,Value,_),       
  retractall(known(no,Attribute,Value,_)),
  assert(known(no,Attribute,Value,X)), !, fail.                        

% if we get here, we need to ask.
ask(A,V):-
  write(A: V),   
  write('? '),
  read(Answer),
  assert(known(Answer,A,V,_)),
  Answer = yes.       

%_____SHELL SECTION_____

top_goal(X):- virus(X).

solve :- 
    abolish(known, 4),  
    define(known, 4),  
    top_goal(X),  
    write('The answer is '), mywrite(X), write('Type of virus: '), kind(X), nl. 
solve :- 
  write('No answer found, would you like to add new entry to knowledge base? '), read(Ans), newentry(Ans). 

newentry(no).
newentry(yes):- createentry.


go:-greeting,repeat,write('> '),
    read(X),
    do(X),
    X==quit.
greeting:-
    mywrite('Welcome to simple expert sytem'), 
    mywrite('I will try to diagnose virus, please answer with yes or no.'), 
    mywrite('Commands you can use in this program(remember to end command with ".").'), 
    mywrite('[help] Show this menu.'),
    mywrite('[consult] Start consulting.'), 
    mywrite('[load] Load knowledge base. It must be in same directory as program named "viruses.txt"'), 
    mywrite('[showyes] Show "yes" answers.'), 
    mywrite('[showno] Show "no" answers.'), 
    mywrite('[quit] Quit.').

do(help):- greeting,!.    
do(load):-loadkb,!.
do(consult):-solve,!.
do(showyes):-yes_answers,!.
do(showno):-no_answers,!.
do(quit):- mywrite('Goodbye.'). 
do(X):- X\=showyes, X\=showno, mywrite('Nieprawidlowa komenda'),fail.

yes_answers:-mywrite('You answered yes for: '),known(yes,X,Y,_),mywrite(X),write(' '),((Y\=='yes',mywrite(Y));(nl)),fail. 
no_answers:-mywrite('You answered no for:'),known(no,X,Y,_),mywrite(X),write(' '),((Y\=='yes',mywrite(Y));(nl)),fail.


%_____Knowledge base loading section_____

loadkb:-
    open('viruses.txt', read, Str),
    read_file(Str),
    close(Str), mywrite('Knowledge base loaded!').

read_file(Stream):-
    at_end_of_stream(Stream), !.

read_file(Stream):-
    \+ at_end_of_stream(Stream),
    read(Stream, X),
    assertz(X),
    read_file(Stream).

%_____New entry handling_____    
createentry:-
    mywrite('Write new name: '), read(Name),
    append('viruses.txt'), nl, write('virus('), write(Name), write('):-'), nl, told,
    addsymptom.

addsymptom:-
    mywrite('Write new symptom name: '), read(S),
    append('viruses.txt'), write(symptom(S)), told,
    mywrite('Add more?'), read(Ans), addmore(Ans).

addmore(yes):-
    mywrite('Write new symptom name: '), read(S),
    append('viruses.txt'), write(,), nl, write(symptom(S)), told,
    mywrite('Add more?'), read(Ans), addmore(Ans).

addmore(no):-
    mywrite('Add source?'), read(Ans), addsource(Ans).

addsource(yes):-
    mywrite('Write new source name'), read(S),
    append('viruses.txt'), write(,), nl, write(source(S)), told,
    mywrite('Add more?'), read(Ans), addsource(Ans).

addsource(no):-
    append('viruses.txt'), write(.), nl, told,
    mywrite('Entry added!').

%_____Cosmetics_____
mywrite(Input):-write(Input),nl, sleep(0.06).

