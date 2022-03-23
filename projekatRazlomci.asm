data SEGMENT ;definicija data segmenta
    brojilac1 dw 0
    znakBrojioca1 dw 0
    znakOperacije dw 0 ;sabiranje, ako je 1 - oduzimanje
    znakRezultata dw 0 ;ako je pozitivan - 0, za negativan rezultat - 1
    imenilac1 dw 0
    brojilac2 dw 0
    imenilac2 dw 0
    brojilacRez dw 0
    imenilacRez dw 0
    nzd dw 0
    nzs dw 0
    izraz db '                      '
    znak db '/$'
    minus db '-$'
    nzsStr db '    '
    nzdStr db '    '
    brojilac1Str db '    '
    imenilac1Str db '    '
    brojilac2Str db '    '
    imenilac2Str db '    '
    brojilacRezStr db '    '
    imenilacRezStr db '    '
    rezultatSting db '       '
    pocetnaPoruka db '==================================RAZLOMCI==================================$'
    poruka1 db 'Unesite izraz:$'
    poruka2 db 'Rezultat je: $'
    porukaKraj db 'Pritisnite neki taster za kraj...$'

data ENDS
stack SEGMENT ;definicija stek segmenta
    dw 256 dup(0) ;navodimo koje velicine ce nam biti stek
stack ENDS
keypress MACRO
    push ax
    mov ah, 08; podprekid prekida koji nam omogucava ovu funkcionalnost je vrednost 08 u ah registru
    int 21h
    pop ax
ENDM
;makro sa parametrom za ispis prosledjenog stringa na ekran
writeString MACRO s
    push ax
    push dx  
    mov dx, offset s
    mov ah, 09 ;podprekid prekida 21h koji nam omogucava ovu funkcionalnost je vrednost 09 u ah registru
    int 21h
    pop dx
    pop ax
ENDM
;makro koji se poziva za zavrsetak programa           
krajPrograma MACRO
    mov ax, 4c02h ;podprekid prekida 21h koji nam omogucava da predamo kontrolu OS-u je vrednost 4c02h u ax registru
    int 21h
ENDM

code SEGMENT;definicija code segmenta
;navodimo prvo procedure koje cemo koristiti, kasnije smestamo vrednosti data i stack segmenta i radimo dalje
;procedura koju pozivamo kada zelimo novi red u konzoli
noviRed PROC
    push ax  ;stavljamo ax, bx, cx i dx registar na stek jer cemo s njima raditi u ovoj proceduri
    push bx
    push cx
    push dx
    mov ah, 03 ;podprekid prekida 10h koji nam omogucava da procitamo trenutnu poziciju kursora na konzoli
    mov bh, 0
    int 10h
    inc dh
    mov dl, 0
    mov ah, 02 ;podprekid prekida 10h koji nam omogucava da postavimo kursor na konzoli u vrsti iz dh registra i koloni iz dl registra
    int 10h
    pop dx  ;skidamo dx, cx, bx i ax registre sa steka (u obrnutom redosledu u odnosu na koji smo ih stavili jer stek radi po LIFO principu)
    pop cx
    pop bx
    pop ax  
    ret
noviRed ENDP
;ucitavanje stringa sa tastature, adresa stringa je parametar na steku
readString PROC
    push ax  ;stavljamo ax, bx, cx, dx i si registar na stek jer cemo s njima raditi u ovoj proceduri
    push bx
    push cx
    push dx
    push si
    mov bp, sp
    mov dx, [bp+12] ;offsetna adresa stringa (preskacemo 10 bajtova za registre i jos 2 bajta za povratnu adresu procedure)
    mov bx, dx
    mov ax, [bp+14]
    mov byte [bx] ,al
    mov ah, 0Ah ;podprekid prekida 21h koji omogucava da korisnik unese string
    int 21h
    mov si, dx   
    mov cl, [si+1] ;smestamo broj ucitanih znakova u registar cl
    mov ch, 0
kopiraj: ;moramo ucitane znakove pomeriti dva mesta u levo i to radimo u petlji kopiraj
    mov al, [si+2]
    mov [si], al
    inc si
    loop kopiraj     
    mov [si], '$' ;kada ispomeramo sve znakove, dodajemo $ na kraju string kako bi on bio validnog formata
    pop si  
    pop dx
    pop cx
    pop bx
    pop ax
    ret 4
readString ENDP
stringToInt PROC
    push ax
    push bx
    push cx
    push dx
    push si
    mov bp, sp
    mov bx, [bp+14]
    mov ax, 0
    mov cx, 0
    mov si, 10
petlja1: ;idemo karakter po karakter po stringu sve dok ne dodjemo do $, njega onda odsecamo i dobijamo broj
    mov cl, [bx]
    cmp cl, '$'
    je kraj1
    mul si
    sub cx, 48
    add ax, cx
    inc bx  
    jmp petlja1
kraj1:
    mov bx, [bp+12] 
    mov [bx], ax 
    pop si  
    pop dx
    pop cx
    pop bx
    pop ax
    ret 4 
stringToInt ENDP
; Konvertuje broj u string (da bi neka cifra bila konvertovana u string, po ASCII tabeli joj se dodaje 48 
; npr. cifra 1 po ASCII tabeli ima vrednost 1, a  karaktekte 1 (string duzine 1) ima vrednost 49
intToString PROC
   push ax
   push bx
   push cx
   push dx
   push si
   mov bp, sp
   mov ax, [bp+14] 
   mov dl, '$'
   push dx
   mov si, 10
petlja2:
   mov dx, 0
   div si
   add dx, 48
   push dx
   cmp ax, 0
   jne petlja2   
   mov bx, [bp+12]
petlja2a:      
   pop dx
   mov [bx], dl
   inc bx
   cmp dl, '$'
   jne petlja2a
   pop si  
   pop dx
   pop cx
   pop bx
   pop ax 
   ret 4
intToString ENDP
procitajBrojilac PROC
    push ax
    pet1:
        mov al, [si]
        cmp al, '-' ;kad proveravamo prvi brojilac, gledamo da li je negativnog znaka
        je negativanBrojilac
        jmp nastavakPetlje1
    negativanBrojilac:
        mov znakBrojioca1, 1
        inc si
        jmp nastavakPetlje1
    nastavakPetlje1: ;setamo se po izrazu dok ne dodjemo do / i to nam je brojilac
        xor al, al
        mov al, [si]
        cmp al, '/'
        je krajProc1
        mov [di], al
        inc si
        inc di
        jmp nastavakPetlje1
    krajProc1:
        mov [di], '$'
        pop ax
        ret   
ENDP
procitajImenilac1 PROC
    push ax
    pet2:
        mov al, [si]
        cmp al,'-' ;ukoliko smo procitali imenilac, gledamo koja nam je operacija izmedju razlomaka
        je oduzimanje  ;oduzimanje
        cmp al, '+'  ;sabiranje
        je krajProc2 
        mov [di], al ; u suprotnom smestamo u di i setamo se dalje po izrazu
        xor al, al
        inc si
        inc di
        jmp pet2   
        oduzimanje:
            mov znakOperacije, 1
            xor ax, ax
            jmp krajProc2
    krajProc2:      
        mov [di], '$'
        pop ax
        ret
ENDP
procitajImenilac2 PROC
    push ax
    pet3:
        mov al, [si]
        cmp al,'$' ;citamo drugi imenilac sve dok ne dodjemo do kraja izraza
        je krajProc3
        mov [di], al
        xor ax, ax
        inc si
        inc di
        jmp pet3
    krajProc3:
        mov [di], '$'
        pop ax
        ret
ENDP
odredjivanjeNZD PROC
     push ax
     push bx
     push bp 
     mov bp, sp
     mov ax, [bp+8]
     mov bx, [bp+10]
     cmp ax, bx ;ukoliko su parametri procedure smesteni u ax i bx registar jednaki, kraj procedure
     je NZDKraj
     jg vece ;ukoliko je vrednost ax veca od vrednosti bx registra
     sub bx, ax ;ukoliko je vrednost bx veca od vrednosti ax registar, oduzmemo ax od bx i nastavljamo dalje
     jmp nastavi
    vece:
        sub ax, bx
    nastavi: 
        push ax ;ovde ce biti smestena vrednost NZD-a
        push ax
        push bx
        call odredjivanjeNZD ;idemo dalje u rekurziju
        pop ax ;kada se vratimo iz rekurzije, skidamo ax sa steka jer nam je tu smesten trazeni NZD
    NZDKraj:
        mov [bp+12], ax  ;kada se potpuno vratimo iz rekurzije, vrednost ax smestamo u prvi parametar nase procedure
        pop bp
        pop bx
        pop ax
        ret 4
ENDP
odredjivanjeNZS PROC  ;NZS racunamo po formuli imenilac1/NZD(imenilac1, imenilac2) * imenilac2
    push ax
    push bx
    push cx
    push dx
    push bp
    mov bp, sp   
    mov ax, [bp+14]
    mov bx, [bp+16]
    mov dx, 0
    div bx
    xor bx, bx
    mov bx, [bp+12]
    mul bx
    NZSKraj:
        mov [bp+18], ax
        pop bp
        pop dx
        pop cx
        pop bx
        pop ax
        ret 6    
ENDP
racunanjeBrojiocaRez PROC
   push ax
   push bx
   push cx
   push dx
   push bp
   mov bp, sp
   mov ax, [bp+16] ;imenilac1
   mov bx, [bp+14] ;imenilac2
   cmp ax, bx
   je jednakiImenioci  ;ukoliko su imenioci jednaki, to je i rezultujuci imenilac
   mov ax,[bp+12] ;NZS(imenilac1, imenilac2)
   mov bx, [bp+16] ;imenilac1
   cmp ax, bx
   je imenilacRez1 ;ukoliko je imenilac1 jednak NZSu
   mov bx, [bp+14] ;imenilac2
   cmp ax, bx
   je imenilacRez2  ;ukoliko je imenilac2 jednak NZSu
   jmp nastavakImenioca ;ukoliko nijedan imenilac nije jednak NZSu
   
   jednakiImenioci:
        mov [bp+22], ax
        xor ax, ax
        jmp racunanjeDalje
   imenilacRez1:
        mov [bp+22], ax ;imenilacRez je NZS
        xor ax, ax
        mov ax, [bp+12]
        xor bx, bx
        mov bx, [bp+14[
        div bx ;rezultat deljenja je sacuvan u ax registru (sa tim mnozimo brojilac2) - NZS / imenilac2
        xor bx, bx
        mov bx, [bp+18] ;brojilac2
        mul bx ;rezultat brojioca2 posle mnozenja je u ax
        mov [bp+18], ax
        jmp racunanjeDalje
   imenilacRez2:
        mov [bp+22], ax ;imenilacRez je NZS
        xor ax, ax
        mov ax, [bp+12]
        mov bx, [bp+16]
        div bx ;rezultat deljenja NZS sa imeniocem1 je u ax (sa tim mnozimo brojilac1)
        xor bx, bx
        mov bx, [bp+20] ;brojilac1
        mul bx ;rezultat brojioca1 posle mnozenja sa ax
        mov [bp+20], ax
        jmp racunanjeDalje      
   nastavakImenioca:
        mov [bp+22], ax  ;imenilacRez je NZS
        xor ax, ax
        mov ax, [bp+12]
        xor bx, bx
        mov bx, [bp+16]
        xor dx, dx
        div bx ; sa koliko treba da mnozimo brojioc1 (ta vrednost je u ax registru) 
        xor bx, bx
        mov bx, [bp+20]
        mul bx ; u ax je sad vrednost brojioca1, pa smestamo u promeljivu bp+20 gde je brojilac1
        mov [bp+20], ax
        ;analogno uradimo za brojilac2
        xor ax, ax  
        mov ax, [bp+12]
        xor bx, bx
        mov bx, [bp+14]
        xor dx, dx
        div bx; sa koliko treba da mnozimo brojilac2
        xor bx, bx
        mov bx, [bp+18]
        mul bx; u ax je nova vrednost brojioca1 pa smestamo u promenljvu bp+18 gde je brojilac2
        mov [bp+18], ax
        jmp racunanjeDalje   
   racunanjeDalje:
        xor cx, cx
        mov cx, znakBrojioca1 ;gledamo koji je znak prvog brojioca
        cmp cx, 0
        je pozitivanPrvi ;brojilac1 je pozitivan
        jmp negativanPrvi ;brojilac1 je negativan
        pozitivanPrvi:
            xor cx, cx
            mov cx, znakOperacije ;gledamo da li nam je sabiranje ili oduzimanje razlomaka
            cmp cx, 0
            je slucajPlusPlus
            jmp slucajPlusMinus
        negativanPrvi:
            xor cx, cx
            mov cx, znakOperacije
            cmp cx, 0
            je slucajMinusPlus
            jmp slucajMinusMinus
             
        slucajPlusPlus: ; slucaj kada imamo izraz oblika R1+R2, radi se klasicno sabiranje dva razlomka
            xor ax, ax
            xor bx, bx
            xor cx, cx
            mov ax, [bp+20] ;brojilac1
            mov bx, [bp+18] ;brojilac2
            add ax, bx ; u ax je sada rezultat brojilacRez, pa tu i smestimo
            mov [bp+24], ax
            jmp krajProcedure
            
        slucajPlusMinus:  ;slucaj kada imamo izraz oblika R1-R2
            xor ax, ax
            xor bx, bx
            xor cx, cx
            mov ax, [bp+20] ;brojilac1
            mov bx, [bp+18] ;brojilac2
            cmp ax, bx  ;gledamo koji je brojilac veci
            je jednaki ;jednaki su
            jg veciPrvi ;veci je prvi
            jmp veciDrugi ;veci je drugi
               
        slucajMinusPlus: ;slucaj kada imamo izraz oblika -R1+R2
            xor ax, ax
            xor bx, bx
            xor cx, cx
            mov ax, [bp+18] ;brojilac2
            mov bx, [bp+20] ;brojilac1
            cmp ax, bx  ;gledamo koji je brojilac veci
            jg veciDrugi2
            jl veciPrvi2
            jmp jednaki
        veciPrvi2: ;resenje je negativno, od prvog oduzimamo drugi
            sub bx, ax
            mov [bp+24], bx
            mov znakRezultata, 1
            xor ax, ax
            xor bx, bx
            jmp krajProcedure   
        veciDrugi2: ;resenje je pozitivno, od drugog oduzimamo prvi brojilac
            sub ax, bx
            mov [bp+24], ax
            xor ax, ax
            xor bx, bx
            jmp krajProcedure
                       
        slucajMinusMinus: ;slucaj kada imamo izraz oblika -R1-R2
            xor ax, ax
            xor bx, bx
            xor cx, cx
            mov ax, [bp+20] ;brojilac 1
            mov bx, [bp+18] ;brojilac 2
            add ax, bx  ;samo cemo sabrati brojioce
            mov [bp+24], ax ;smestiti rezultat u brojilacRez
            mov znakRezultata, 1  ;i znak rezultata cemo postaviti na negativan
            xor ax, ax
            xor bx, bx
            jmp krajProcedure     
        veciPrvi: ;resenje je pozitivno, od prvog oduzimamo drugi
            sub ax, bx
            mov [bp+24], ax
            xor ax, ax
            xor bx, bx
            jmp krajProcedure
        veciDrugi: ;oduzimamo od drugog prvi, a znak resenja ce biti negativan
            sub bx, ax
            mov [bp+24], bx
            mov znakRezultata, 1
            xor ax, ax
            xor bx, bx
            jmp krajProcedure      
   
        jednaki: ;brojioci su jednaki, oduzimanjem ce dati rezultat nula
            mov [bp+24], 0
            jmp krajProcedure                    
   krajProcedure:
        pop bp
        pop dx
        pop cx
        pop bx
        pop ax
        ret 6                         
ENDP
noviRezultat PROC  ;skraceni rezultat
    push ax
    push bx
    push dx
    push bp
    mov bp, sp
    mov ax, [bp+12]
    mov bx, [bp+10]
    xor dx, dx
    div bx ;delimo imenilac sa NZD(brojilacRez, imenilacRez)
    mov [bp+12], ax ;smestamo novi imenilac u promenljivu
    xor ax, ax
    xor bx, bx
    mov ax, [bp+14]
    mov bx, [bp+10]
    mov dx, 0
    div bx ;delimo brojilac za NZD
    mov [bp+14], ax ;smestamo novu vrednost brojioca u promenljivu
    xor ax, ax
    xor bx, bx
    xor dx, dx  
    pop bp
    pop dx
    pop bx
    pop ax
    ret 2        
ENDP
start:
    ASSUME cs:code, ss:stack
    mov ax, data
    mov ds, ax
    call novired
    writeString pocetnaPoruka ;ispisemo pocetnu poruku na ekran 
    call novired
unos:
    call novired
    writeString poruka1 ;poruka da korisnik unese string za koji se proverava da li je palindrom
    push 20
    push offset izraz  
    call readString ;procitamo uneti izraz i pocetna adresa mu je offset izraz
smestanje:
    mov si, offset izraz 
    mov di, offset brojilac1Str
    push si
    push di
    call procitajBrojilac ;procitamo brojilac iz izraza
    inc si
    xor di, di
    mov di, offset imenilac1Str
    push si
    push di
    call procitajImenilac1  ;procitamo prvi imenilac iz izraza
    inc si
    xor di, di
    mov di, offset brojilac2Str
    push si
    push di
    call procitajBrojilac  ;procitamo drugi brojilac
    inc si
    xor di, di
    mov di, offset imenilac2Str
    push si
    push di
    call procitajImenilac2 ;procitamo drugi imenilac
konvertovanje:
    push offset brojilac1Str
    push offset brojilac1
    call stringToInt
    push offset imenilac1Str
    push offset imenilac1
    call stringToInt
    push offset brojilac2Str
    push offset brojilac2
    call stringToInt
    push offset imenilac2Str
    push offset imenilac2
    call stringToInt
trazenjeNZD: ;trazimo NZD(imenilac1, imenilac2)
    push nzd
    push imenilac1
    push imenilac2
    call odredjivanjeNZD
    pop nzd    
trazenjeNZS: ;trazimo NZS(imenilac1, imenilac2) - pomocu NZD
    push nzs
    push nzd 
    push imenilac1
    push imenilac2
    call odredjivanjeNZS
    pop nzs
krajnjiRezultat:
    push brojilacRez
    push imenilacRez
    push brojilac1
    push brojilac2
    push imenilac1
    push imenilac2
    push nzs
    call racunanjeBrojiocaRez ;racunamo krajnji rezultat
    pop brojilac2
    pop brojilac1
    pop imenilacRez
    pop brojilacRez
nastavakProvera:
    call noviRed
    call noviRed
    writeString poruka2
    mov ax, brojilacRez
    cmp ax, 0 ;ukoliko je rezultat brojioca 0, samo cemo to ispisati
    je samoBrojilacIspis
    xor ax, ax
    push nzd
    push brojilacRez
    push imenilacRez
    call odredjivanjeNZD  ;u suprotnom,trazimo NZD(brojilacRez, imenilacRez) ukoliko mozemo skratiti nas razlomak
    pop nzd
    push brojilacRez
    push imenilacRez
    push nzd
    call noviRezultat ;smestimo novi rezultat u promenljive
    pop imenilacRez
    pop brojilacRez
pretvaranje:    
    push brojilacRez
    push offset brojilacRezStr
    call intToString
    push imenilacRez
    push offset imenilacRezStr
    call intToString
pocetakIspisa:
    xor ax, ax
    mov ax, znakRezultata
    cmp ax, 1
    je ispisiMinus  ;ukoliko je znak rezultata -, prvo cemo njega ispisati
    jmp ispisBrojeva ;u suprotnom, odmah idemo na ispis brojeva

ispisiMinus:  ;labela za ispisivanje minusa
    xor ax, ax
    writeString minus
        
ispisBrojeva:
    xor ax, ax
    mov ax, imenilacRez
    cmp ax, 1
    je samoBrojilacIspis  ;ukoliko je imenilac 1, samo cemo brojilac da ispisemo    
    ispisA: ;u suprotnom ispisujemo razlomak oblika B/I
        xor ax, ax
        writeString brojilacRezStr
        writeString znak
        writeString imenilacRezStr
        jmp kraj            
samoBrojilacIspis:
    xor ax, ax
    push brojilacRez
    push offset brojilacRezStr
    call intToString
    writeString brojilacRezStr
kraj: ;labela za zavrsetak programa
    call noviRed
    call noviRed                          
    writeString porukaKraj    
    keypress 
    krajPrograma        
ENDS
end start  