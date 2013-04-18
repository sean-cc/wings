%%
%%  wings_dialog.erl --
%%
%%     This module implements the dialogs.
%%
%%  Copyright (c) 2013 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wings_dialog).
-include("wings.hrl").
-export([info/3, 
	 ask/3, ask/4, 
	 dialog/3, dialog/4]).

-record(in, {key, type, def, wx, validator, data}).

info(Title, Info, _Options) ->
    Parent = get(top_frame),
    Style  = {style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER},
    Size   = {size, {500, 400}},
    Dialog = wxDialog:new(Parent, ?wxID_ANY, Title,
			  [Style, Size]),
    Panel  = wxHtmlWindow:new(Dialog, []),
    Sizer  = wxBoxSizer:new(?wxVERTICAL),
    Html = text_to_html(Info),
    wxHtmlWindow:appendToPage(Panel, Html),
    wxSizer:add(Sizer, Panel, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxWindow:setSizer(Dialog, Sizer),
    wxDialog:showModal(Dialog),
    wxDialog:destroy(Dialog),
    keep.

%% Currently a modal dialog
%%   (orginal wings let camera events trough)
ask(Title, Qs, Fun) ->
    dialog(true, Title, queries(Qs), Fun).
ask(Bool, Title, Qs, Fun) ->
    dialog(Bool, Title, queries(Qs), Fun).

dialog(Title, Qs, Fun) ->
    dialog(true, Title, Qs, Fun).
dialog(Bool, Title, Qs, Fun) ->
    {Dialog, Fields} = build_dialog(Bool, Title, Qs),
    case Bool andalso wxDialog:showModal(Dialog) of
	?wxID_CANCEL -> keep;
	_Ok ->
	    Values = [get_output(Bool, Field) || Field <- Fields],
	    Bool andalso wxDialog:destroy(Dialog),
	    return_result(Fun, Values)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers 

get_output(false, In=#in{def=Def}) -> 
    with_key(In, Def);
get_output(_, In=#in{type=checkbox, wx=Ctrl}) ->
    with_key(In,wxCheckBox:getValue(Ctrl));
get_output(_, In=#in{type=radiobox, wx=Ctrl, data=Keys}) ->
    ZeroIndex = wxRadioBox:getSelection(Ctrl),
    with_key(In, lists:nth(ZeroIndex+1, Keys));
get_output(_, In=#in{type=filepicker, wx=Ctrl}) ->
    with_key(In,wxFilePickerCtrl:getPath(Ctrl));
get_output(_, In=#in{type=text, def=Def, wx=Ctrl, validator=Validate}) ->
    Str = wxTextCtrl:getValue(Ctrl),
    Res = if is_integer(Def) ->
		  wings_ask:eval_integer(validate(Validate, Str));
	     is_float(Def)  ->
		  wings_ask:eval_float(validate(Validate, Str));
	     is_list(Def) ->
		  Str
	  end,
    with_key(In, Res).

with_key(#in{key=undefined}, Value) -> Value;
with_key(#in{key=Key}, Value) ->  {Key, Value}.
   
validate(Fun, Input) ->
    case Fun(Input) of
	ok -> Input;
	Str when is_list(Str) -> Str
    end.

return_result(Fun, Values) ->
    Owner = wings_wm:this(),
    case Fun(Values) of
	ignore ->
	    ok;
	#st{}=St ->  
	    wings_wm:send(Owner, {new_state,St});
	{commit,#st{}=St0,#st{}=St}->
	    wings_wm:send(Owner, {current_state,St0}),
	    wings_wm:send_after_redraw(Owner, {new_state,St});
	{commit,Action,#st{}=St}->
	    wings_wm:send(Owner, {current_state,St}),
	    wings_wm:send_after_redraw(Owner, {action,Action});
	Action when is_tuple(Action); is_atom(Action) ->
	    wings_wm:send(Owner, {action,Action})
    end,
    keep.

queries(Qs0) ->
    {Labels,Vals} = ask_unzip(Qs0),
    [{hframe,
	   [{vframe,Labels},
	    {vframe,Vals}]}].

ask_unzip(Qs) ->
    ask_unzip(Qs, [], []).
ask_unzip([{Label,{menu,_,_}=Menu}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [Menu|AccB]);
ask_unzip([{Label,{menu,_,_,_}=Menu}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [Menu|AccB]);
ask_unzip([{Label,Def}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [{text,Def}|AccB]);
ask_unzip([{Label,Def,Flags}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [{text,Def,Flags}|AccB]);
ask_unzip([], Labels, Vals) ->
    {lists:reverse(Labels),lists:reverse(Vals)}.

build_dialog(false, _Title, Qs) ->
    Fs = build(false, {vframe, Qs}, undefined, undefined, []),
    {undefined, Fs};
build_dialog(true, Title, Qs) ->
    Parent = get(top_frame),
    Style  = {style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER},
    Dialog = wxDialog:new(Parent, ?wxID_ANY, Title, [Style]),
    Panel  = wxPanel:new(Dialog, []),
    Top    = wxBoxSizer:new(?wxVERTICAL),
    Fields = case is_list(Qs) of 
		 true -> 
		     Sz = wxBoxSizer:new(?wxVERTICAL),
		     Fs = build(true, {vframe, Qs}, Panel, Sz, []),
		     wxWindow:setSizer(Panel, Sz),
		     wxSizer:setSizeHints(Sz, Panel),
		     wxSizer:add(Top, Panel, [{proportion, 1}, 
					      {flag, ?wxEXPAND bor ?wxALL},
					      {border, 5}
					     ]),
		     Ok = wxDialog:createButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
		     wxSizer:add(Top, Ok, [{flag, ?wxEXPAND bor ?wxALL}, 
					   {border, 5}]),
		     Fs;
		 false ->
		     build(true, Qs, Panel, Top, [])
	     end,
    wxWindow:setSizerAndFit(Dialog, Top),
    wxSizer:setSizeHints(Top, Dialog),
    {Dialog, lists:reverse(Fields)}.

build(Ask, {vframe, Qs}, Parent, Sizer, In) ->
    build(Ask, {vframe, Qs, []}, Parent, Sizer, In);
build(Ask, {vframe, Qs, Flags}, Parent, Sizer, In) ->
    build_box(Ask, ?wxVERTICAL, Qs, Flags, Parent, Sizer, In);
build(Ask, {hframe, Qs}, Parent, Sizer, In) ->
    build(Ask, {hframe, Qs, []}, Parent, Sizer, In);
build(Ask, {hframe, Qs, Flags}, Parent, Sizer, In) ->
    build_box(Ask, ?wxHORIZONTAL, Qs, Flags, Parent, Sizer, In);

build(Ask, {vradio, Alternatives, Def}, Parent, Sizer, In) ->
    build(Ask, {vradio, "", Alternatives, Def}, Parent, Sizer, In);
build(Ask, {vradio, Name, Alternatives, Def}, Parent, Sizer, In) ->
    build_radio(Ask, Name, Def, ?wxRA_SPECIFY_COLS, Alternatives, Parent, Sizer, In);
build(Ask, {hradio, Alternatives, Def}, Parent, Sizer, In) ->
    build(Ask, {hradio, "", Alternatives, Def}, Parent, Sizer, In);
build(Ask, {hradio, Name, Alternatives, Def}, Parent, Sizer, In) ->
    build_radio(Ask, Name, Def, ?wxRA_SPECIFY_ROWS, Alternatives, Parent, Sizer, In);

build(true, {label, Label}, Parent, Sizer, In) ->
    Text = wxStaticText:new(Parent, ?wxID_ANY, Label),
    wxSizer:add(Sizer, Text, [{proportion,1}, {flag, ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL}]),
    In;
build(true, panel, _Parent, Sizer, In) ->
    wxSizer:addSpacer(Sizer, 20),
    In;
build(true, separator, Parent, Sizer, In) ->
    Separator = wxStaticLine:new(Parent),
    wxSizer:add(Sizer, Separator, 
		[{proportion,0}, {border, 5},
		 {flag, ?wxALL bor ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL}]),
    In;

build(Ask, {text, Def}, Parent, Sizer, In) ->
    build(Ask, {text, Def, []}, Parent, Sizer, In);
build(Ask, {text, Def, Flags}, Parent, Sizer, In) ->
    {_Max0,Validator,_Charset} = wings_ask:validator(Def, Flags),
    Create = fun() ->
		     Ctrl = wxTextCtrl:new(Parent, ?wxID_ANY, [{value, to_str(Def)}]),
		     wxSizer:add(Sizer, Ctrl, [{proportion,0}, {flag, ?wxEXPAND}]),
		     Ctrl
	     end,
    [#in{key=proplists:get_value(key,Flags), def=Def,
	 type=text, wx=create(Ask,Create), validator=Validator}|In];
build(Ask, {button, {text, Def, Flags}}, Parent, Sizer, In) ->
    Create = fun() ->
		     Props = proplists:get_value(props, Flags, []),
		     What = case proplists:get_value(dialog_type, Props, open_dialog) of
				open_dialog -> ?wxFLP_OPEN;
				save_dialog -> ?wxFLP_SAVE
			    end,
		     Filter = wings_file:file_filters(Props),
		     Ctrl = wxFilePickerCtrl:new(Parent, ?wxID_ANY, 
						 [{style, What bor ?wxFLP_USE_TEXTCTRL},
						  {path, Def}, 
						  {wildcard, Filter}]), 
		     wxSizer:add(Sizer, Ctrl, [{proportion,0}, {flag, ?wxEXPAND}]),
		     Ctrl
	     end,
    [#in{key=proplists:get_value(key,Flags), def=Def,
	 type=filepicker, wx=create(Ask,Create)}|In];

build(Ask, {table, [Header|Rows], Flags}, Parent, Sizer, In) ->
    Create = 
	fun() ->
		Options = [{style, ?wxLC_REPORT}, 
			   {size, {min(tuple_size(Header)*80, 500), 
				   min((2+length(Rows))*25, 800)}}],
		Ctrl = wxListCtrl:new(Parent, Options),
		AddHeader = fun(HeadStr, Column) ->
				    wxListCtrl:insertColumn(Ctrl, Column, HeadStr, []),
				    Column + 1
			    end,
		lists:foldl(AddHeader, 0, tuple_to_list(Header)),
		case proplists:get_value(col_widths, Flags) of
		    undefined -> ok;
		    Widths ->
			SetWidth = fun(Width, Column) ->
					   wxListCtrl:setColumnWidth(Ctrl, Column, Width*8),
					   Column + 1
				   end,
			lists:foldl(SetWidth, 0, tuple_to_list(Widths))
		end,
		Add = fun({_, Str}, {Row, Column}) ->
			      wxListCtrl:setItem(Ctrl, Row, Column, Str),
			      {Row, Column+1}
		      end,
		lists:foldl(fun(Row, N) ->
				    wxListCtrl:insertItem(Ctrl, N, ""),
				    lists:foldl(Add, {N, 0}, tuple_to_list(Row)),
				    N + 1
			    end, 0, Rows),
		wxSizer:add(Sizer, Ctrl, [{proportion,1}, {flag, ?wxEXPAND}]),
		Ctrl
	end,
    create(Ask,Create),
    %% [#in{key=proplists:get_value(key,Flags), def=Rows,
    %% 	 type=table, wx=create(Ask,Create)}|In];
    In;

build(Ask, {Label, Def}, Parent, Sizer, In) ->
    build(Ask, {Label, Def, []}, Parent, Sizer, In);
build(Ask, {Label, Def, Flags}, Parent, Sizer, In) 
  when is_boolean(Def) ->
    Create = fun() ->
		     Ctrl = wxCheckBox:new(Parent, ?wxID_ANY, Label),
		     wxCheckBox:setValue(Ctrl, Def),
		     wxSizer:add(Sizer, Ctrl, [{proportion,1}, {flag, ?wxEXPAND}]),
		     Ctrl
	     end,
    [#in{key=proplists:get_value(key,Flags), 
	 def=Def, type=checkbox, wx=create(Ask, Create)}|In];

build(false, _Q, _Parent, _Sizer, In) -> 
    In;
build(Ask, Q, _Parent, _Sizer, In) ->
    io:format("~p:~p: Unhandled ask=~p, ~p~n",[?MODULE,?LINE,Ask,Q]),
    In.

build_box(true, Type, Qs, _Flags, Parent, Top, In0) ->
    Sizer = wxBoxSizer:new(Type),
    Input = lists:foldl(fun(Q, Input) ->
				build(true, Q, Parent, Sizer, Input)
			end, In0, Qs),
    wxSizer:add(Top, Sizer, [{proportion,1}, {flag, ?wxEXPAND}]),
    Input;
build_box(false, _Type, Qs, _, Parent, _Top, In0) ->
    lists:foldl(fun(Q, Input) ->
			build(false, Q, Parent, undefined, Input)
		end, In0, Qs).

build_radio(Ask, Name, Def, Direction, Alternatives, Parent, Sizer, In) ->
    {Strs,Keys} = lists:unzip(Alternatives),
    true = lists:member(Def, Keys),
    Create = fun() -> 
		     Ctrl = wxRadioBox:new(Parent, 1, Name,
					   ?wxDefaultPosition, ?wxDefaultSize,
					   Strs, [{majorDim, 1}, {style, Direction}]),
		     wxSizer:add(Sizer, Ctrl, [{proportion,0}, {flag, ?wxEXPAND}]),
		     wxRadioBox:enable(Ctrl, pos(Def, Keys)),
		     Ctrl
	     end,
    [#in{def=Def, type=radiobox, wx=create(Ask, Create), data=Keys}|In].

create(false, _) -> undefined;    
create(true, Fun) -> Fun().

to_str(Number) when is_integer(Number) ->
    integer_to_list(Number);
to_str(Float) when is_float(Float) ->
    wings_util:nice_float(Float);
to_str(List = [C|_]) when is_integer(C) ->
    List.


pos(C, S) -> pos(C, S, 0).
pos(C, [C|_Cs], I) -> I;
pos(C, [_|Cs], I) -> pos(C, Cs, I+1);
pos(_, [], _I) -> 0.

text_to_html(Paragraphs) ->
    Header = ["<html>"],
    Html = text_to_html(Paragraphs, Header),
    lists:reverse(["</html>"|Html]).

text_to_html([[_|_] = Paragraph|Ps], Acc) ->
    text_to_html(Ps, ["</p>", paragraph_to_html(Paragraph), "<p>"|Acc]);
text_to_html([Table = {table,_,_,_}|Ps], Acc) ->
    text_to_html(Ps, [table_to_html(Table)|Acc]);
text_to_html([{bullet, List}|Rest], Acc) ->
    BulletList = ["</p><ul>", 
		  ["<li>" ++ paragraph_to_html(Item) ++ "</li>" || Item <- List],
		  "</ul><p>"],
    text_to_html(Rest, [BulletList|Acc]);
text_to_html([], Acc) -> Acc.

paragraph_to_html([C|Text]) when is_integer(C) ->
    [C|paragraph_to_html(Text)];
paragraph_to_html([{bold, Text}|Rest]) ->
    ["<b>", paragraph_to_html(Text), "</b>"| paragraph_to_html(Rest)];
paragraph_to_html([{ul, Text}|Rest]) ->
    ["<u>", paragraph_to_html(Text), "</u>"| paragraph_to_html(Rest)];
paragraph_to_html([Table={table, _, _, _}|Rest]) ->
    ["</p>", table_to_html(Table), "<p>" | paragraph_to_html(Rest)];
paragraph_to_html([C|Text]) when is_list(C) ->
    [paragraph_to_html(C), paragraph_to_html(Text)];
paragraph_to_html([]) -> [].

table_to_html({table, _, Header, Items}) ->
    ["<p><b>", Header, "</b></p><table>", 
     [table_row_to_html(Row) || Row <- Items],
     "</table>"].

table_row_to_html(Row) when is_list(Row) ->
    ["<tr>", ["<td>" ++ paragraph_to_html(Column) ++ "</td>" || Column <- Row], "</tr>"].
    
