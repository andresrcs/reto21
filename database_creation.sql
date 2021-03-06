-- !preview conn=con

CREATE EXTENSION btree_gist;

CREATE SCHEMA IF NOT EXISTS reto21 AUTHORIZATION ubuntu;
SET search_path TO reto21;

-- Crear tbl_coaches
create type permisos_coach as enum
    ('Administrador', 'Coach', 'Inactivo');
   
create table if not exists tbl_coaches (
    id_coach int generated by default as identity primary key,
    nombre_coach varchar not null unique,
    user_coach varchar not null unique,
    password varchar not null,
    num_celular_coach varchar (9),
    email_coach varchar,
    permiso permisos_coach not null default 'Coach'::permisos_coach,
    notificacion_correo boolean not null default false,
    constraint nombre_valido check (nombre_coach ~ '^.{3,}\s.{3}'),
    constraint usuario_valido check (user_coach ~ '^[a-zA-Z0-9!¡/@#$¿?%^&*"\[\]\{\}<>\(\)=\-_´+`~:;,.€\|]{4,}$'),
    constraint password_valido check (password ~ '^[a-zA-Z0-9!¡/@#$¿?%^&*"\[\]\{\}<>\(\)=\-_´+`~:;,.€\|]+$'),
    constraint celular_valido check (num_celular_coach ~ '^\d{9}$'),
    constraint email_valido check (email_coach ~* '^[A-Za-z0-9._%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$')
);

-- Crear fn check_email_notification: No se puede activar alerta si no hay un email registrado
create or replace function check_email_notification()
	returns trigger as
	$BODY$
	begin
		if new.email_coach is null then 
			new.notificacion_correo := false;
		end if;			
		return new;
	end;
	$BODY$
	language plpgsql volatile;

drop trigger if exists trig_01_tbl_coaches_check_email_notification on tbl_coaches;
create trigger trig_01_tbl_coaches_check_email_notification
	before insert or update of notificacion_correo, email_coach on tbl_coaches
	for each row execute function
	check_email_notification();

insert into tbl_coaches (nombre_coach, user_coach, password, permiso)
	values ('Administrador Inicial', 'admin', '$7$C6..../....l094n9rNQTy1e4.xwl5RP1wPENQZFBobdlL4P6p11s/$DRD2H72lcjNGJlStZb7HtwAYRLay5EdtJOGdQZXaQG5', 'Administrador');
	
-- Crear tbl_retadores
create type sexos as enum
    ('Mujer', 'Hombre');
    
create table if not exists tbl_retadores (
    id_retador int generated by default as identity primary key,
    nombre_retador varchar not null unique,
    fecha_nacimiento date,
    sexo sexos,
    num_celular_retador varchar (9) not null unique,
    talla numeric (3,2),
    nombre_coach varchar not null,
    constraint fk_nombre_coach foreign key (nombre_coach)
		references tbl_coaches (nombre_coach)
		on update cascade on delete restrict,
	constraint nombre_valido check (nombre_retador ~ '^.{3,}\s.{3}'),
    constraint celular_valido check (num_celular_retador ~ '^\d{9}$')
);
   
create index if not exists tbl_retadores_fki_nombre_coach
	on tbl_retadores using btree (nombre_coach asc nulls last);

-- Crear tbl_retos
create table if not exists tbl_retos (
	id_reto int generated by default as identity primary key,
	nombre_reto varchar not null unique,
	duracion_reto daterange not null,
	reto_activo boolean not null default true
);

-- Crear fn check_retadores: No pueden activarse retos con los mismos retadores
create or replace function check_retadores()
	returns trigger as
	$BODY$
	declare
		retadores_activos varchar[];
		flag boolean;
	begin
		select into retadores_activos array_agg(tp.nombre_retador)
		from tbl_retos tr inner join tbl_participacion tp on tr.nombre_reto = tp.nombre_reto 
		where tr.reto_activo = true and not (tr.nombre_reto = new.nombre_reto);
	
		select into flag array_agg(tp.nombre_retador)  && retadores_activos
		from tbl_retos tr inner join tbl_participacion tp on tr.nombre_reto = tp.nombre_reto 
		where tp.nombre_reto = new.nombre_reto; 
		
		if flag
		then
			raise exception 'Retos activos no pueden contener los mismos retadores';
		end if;
		return new;
	end;
	$BODY$
	language plpgsql volatile;

drop trigger if exists trig_01_tbl_retos_check_retadores on tbl_retos;
create trigger trig_01_tbl_retadores_check_retadores
	after insert or update of reto_activo on tbl_retos
	for each row execute function
	check_retadores();
	
-- Crear tbl_participacion
create table if not exists tbl_participacion (
	id_participacion int generated by default as identity primary key,
	nombre_reto varchar not null,
	nombre_retador varchar not null,
	objetivo_participacion varchar,
	constraint fk_nombre_reto foreign key (nombre_reto)
		references tbl_retos (nombre_reto)
		on update cascade on delete restrict,
	constraint fk_nombre_retador foreign key (nombre_retador)
		references tbl_retadores (nombre_retador)
		on update cascade on delete restrict,
	constraint uq_nombre_reto_retador unique (nombre_reto, nombre_retador)
);

create index if not exists tbl_participacion_fki_nombre_reto
	on tbl_participacion using btree (nombre_reto asc nulls last);

create index if not exists tbl_participacion_fki_nombre_retador
	on tbl_participacion using btree (nombre_retador asc nulls last);

-- Crear fn check_reto_unico(): No se puede agregar un retador a mas de 1 reto activo
create or replace function check_reto_unico()
	returns trigger as
	$BODY$
	declare 
		num_retos integer;
	begin
		select into num_retos count(*)
		from (select distinct tp.nombre_reto 
				from tbl_participacion tp inner join tbl_retos tr on tp.nombre_reto = tr.nombre_reto
				where  tr.reto_activo = true and tp.nombre_retador = new.nombre_retador) as temp;
		
		if num_retos > 1
		then
			raise exception 'Retador no puede ser inscrito en mas de 1 reto activo a la vez';
		end if;
		return new;
	end;
	$BODY$
	language plpgsql volatile;

create trigger trig_01_tbl_participacion_check_reto_unico
	after insert or update of nombre_reto, nombre_retador on tbl_participacion
	for each row execute function
	check_reto_unico();

-- Crear fn check_reto_activo: No se puede agregar datos referentes a retos inactivos
create or replace function check_reto_activo()
	returns trigger as
	$BODY$
	declare
		flag boolean;
	begin
		if TG_OP = 'INSERT' or TG_OP = 'UPDATE' then
			select into flag reto_activo
			from tbl_retos tr
			where tr.nombre_reto = new.nombre_reto;
		
			if not(flag)
			then
				raise exception 'No se puede modificar o añadir a retos inactivos';
			end if;
			return new;
		elseif TG_OP = 'DELETE' then
			select into flag reto_activo
			from tbl_retos tr
			where tr.nombre_reto = old.nombre_reto;
		
			if not(flag)
			then
				raise exception 'No se puede modificar retos inactivos';
			end if;
			return old;
		else
			raise exception 'Trigger encountered unknown TG_OP ';
		end if;
	end
	$BODY$
	language plpgsql volatile;

drop trigger if exists trig_02_tbl_participacion_check_reto_activo on tbl_participacion;
create trigger trig_02_tbl_participacion_check_reto_activo
	before insert or delete or update on tbl_participacion
	for each row execute function
	check_reto_activo();

-- Crear tbl_parametros
create type unidades as enum
	('Kg', 'cm', '%', 'U');

create table if not exists tbl_parametros (
	id_parametro int generated by default as identity primary key,
	nombre_parametro varchar not null unique,
	unidad unidades not null
);

insert into tbl_parametros (nombre_parametro, unidad) 
	values 
		('Peso', 'Kg'),
		('Medida Cintura', 'cm');

-- Crear tbl_registro_parametros
create table if not exists tbl_registros_parametros (
	id_registro_parametro int generated by default as identity primary key,
	id_participacion int not null,
	nombre_parametro varchar not null,
	valor_inicial numeric not null,
	valor_final numeric,
	constraint fk_id_participacion foreign key (id_participacion)
    	references tbl_participacion (id_participacion)
    	on update cascade on delete restrict,
    constraint fk_nombre_parametro foreign key (nombre_parametro)
    	references tbl_parametros (nombre_parametro)
    	on update cascade on delete restrict,
    constraint uq_participacion_parametro unique (id_participacion, nombre_parametro)
);

create index if not exists tbl_reg_par_fki_id_participacion 
	on tbl_registros_parametros using btree (id_participacion asc nulls last);

create index if not exists tbl_reg_par_fki_nombre_parametro
	on tbl_registros_parametros using btree (nombre_parametro asc nulls last);

-- Crear tbl_fotos
create type tipo_foto as enum
    ('Frente', 'Perfil', 'Posterior');

create type estado_foto as enum
    ('Inicial', 'Final');
    
create table if not exists tbl_fotos (
    id_foto int generated by default as identity primary key,
    id_participacion int not null,
    tipo tipo_foto not null,
    estado estado_foto not null,
    archivo bytea not null,
    fecha_registro date not null default current_date,
    nombre_coach varchar not null,
    constraint fk_id_participacion foreign key (id_participacion)
    	references tbl_participacion (id_participacion)
    	on update cascade on delete restrict,
    constraint fk_nombre_coach foreign key (nombre_coach)
    	references tbl_coaches (nombre_coach)
    	on update cascade on delete restrict,
    constraint uq_participacion_tipo_estado unique (id_participacion, tipo, estado)
);

create index if not exists tbl_fotos_fki_id_participacion 
	on tbl_fotos using btree (id_participacion asc nulls last);

-- Crear fn check_participacion_activa: No se puede agregar datos referentes a retos inactivos
create or replace function check_participacion_activa()
	returns trigger as
	$BODY$
	declare
		flag boolean;
	begin
		if TG_OP = 'INSERT' or TG_OP = 'UPDATE' then
			select into flag tr.reto_activo
			from tbl_participacion tp inner join tbl_retos tr on tp.nombre_reto = tr.nombre_reto
			where tp.id_participacion = new.id_participacion;
		
			if not(flag)
			then
				raise exception 'No se puede modificar o añadir a retos inactivos';
			end if;
			return new;
		elseif TG_OP = 'DELETE' then
			select into flag tr.reto_activo
			from tbl_participacion tp inner join tbl_retos tr on tp.nombre_reto = tr.nombre_reto
			where tp.id_participacion = old.id_participacion;
		
			if not(flag)
			then
				raise exception 'No se puede modificar retos inactivos';
			end if;
			return old;
		else
			raise exception 'Trigger encountered unknown TG_OP ';
		end if;
	end
	$BODY$
	language plpgsql volatile;

drop trigger if exists trig_01_tbl_fotos_check_participacion_activa on tbl_fotos;
create trigger trig_01_tbl_fotos_check_participacion_activa
	before insert or delete or update of id_participacion, tipo, estado, archivo on tbl_fotos
	for each row execute function
	check_participacion_activa();

-- Crear fn update_register_date
create or replace function update_register_date()
	returns trigger as 
	$BODY$
	begin
		if new.nombre_coach = old.nombre_coach then 
			new.fecha_registro := current_date;
		end if;
		return new;
	end;
	$BODY$
	language plpgsql volatile;

create trigger trig_02_tbl_fotos_update_register_date
	before insert or update on tbl_fotos
	for each row execute function
	update_register_date();

-- Crear tbl_conceptos
create table if not exists tbl_conceptos (
	id_concepto int generated by default as identity primary key,
	concepto varchar not null unique,
	peso_concepto numeric(3,2) not null,
	constraint peso_valido check (peso_concepto between 0 and 1)
);

-- Crear fn check_sum_conceptos: La suma de pesos debe ser igual a 1
create or replace function check_sum_conceptos()
	returns trigger as
	$BODY$
	declare
		sum_pesos tbl_conceptos.peso_concepto%type;
	begin
		select into sum_pesos sum (peso_concepto)
		from tbl_conceptos;
		
		if sum_pesos > 1.0 then
			raise exception 'Definir el peso de % como % excede la suma total por %',
			new.concepto,
			new.peso_concepto,
			sum_pesos - 1;
		end if;
		return new;
	end;
	$BODY$
	language plpgsql volatile;

create trigger trig_01_tbl_conceptos_check_sum_conceptos
	after insert or update on tbl_conceptos
	for each row execute function
	check_sum_conceptos();

insert into tbl_conceptos (concepto, peso_concepto)
	values
		('Actividades', 0.10),
		('Habitos', 0.45),
		('Calificacion', 0.45);

-- Crear tbl_registros_ (parent table)
create table if not exists tbl_registros_ (
	id_participacion int not null,
	concepto varchar not null,
	nombre_coach varchar not null,
	fecha_registro date not null default current_date
);

-- Crear tbl_actividades
create type tipo_actividad as enum
    ('Ceremonia', 'Charla Alimentación', 'Charla Motivación', 'Fit Camp');

create table if not exists tbl_actividades (
	id_actividad int generated by default as identity primary key,
	actividad tipo_actividad not null,
	tiempo_actividad tsrange not null,
	nombre_reto varchar not null,
	tema_actividad varchar not null,
	coach_expositor varchar not null,
	constraint fk_nombre_reto foreign key (nombre_reto)
		references tbl_retos (nombre_reto)
		on update cascade on delete restrict,
	constraint fk_coach_expositor foreign key (coach_expositor)
		references tbl_coaches (nombre_coach)
		on update cascade on delete restrict,
	exclude using gist (nombre_reto with =, tiempo_actividad with &&)
);

create index if not exists tbl_actividades_fki_nombre_reto
	on tbl_actividades using btree (nombre_reto asc nulls last);

create index if not exists tbl_actividades_fki_coach_expositor
	on tbl_actividades using btree (coach_expositor asc nulls last);

create index if not exists tbl_actividades_tiempo_actividad 
	on tbl_actividades using gist (tiempo_actividad);

-- Crear fn check_date_range: Fecha de la actividad debe estar dentro de la duracion del reto
create or replace function check_date_range_actividad()
	returns trigger as
	$BODY$
	declare
		duracion tbl_retos.duracion_reto%type;
	begin	
		select into duracion duracion_reto
		from tbl_retos tr
		where tr.nombre_reto = new.nombre_reto;
		
		if not (new.tiempo_actividad <@ duracion::varchar::tsrange) and (new.actividad != 'Ceremonia'::tipo_actividad) then 
			raise exception 'Cronograma % fuera de rango %',
			new.tiempo_actividad,
			duracion;
		end if;
		return new;
	end;
	$BODY$
	language plpgsql volatile;

drop trigger if exists trig_01_tbl_actividades_check_date_range_actividad on tbl_actividades;
create trigger trig_01_tbl_actividades_check_date_range_actividad
	before insert or update of tiempo_actividad on tbl_actividades
	for each row execute function
	check_date_range_actividad();

drop trigger if exists trig_02_tbl_actividades_check_reto_activo on tbl_actividades;
create trigger trig_02_tbl_actividades_check_reto_activo
	before insert or delete or update of actividad, tiempo_actividad, nombre_reto on tbl_actividades
	for each row execute function
	check_reto_activo();

   -- Crear tbl_registros_actividaes
create table if not exists tbl_registros_actividades (
	id_registro_actividad int generated by default as identity primary key,
	id_actividad int not null,
	constraint fk_id_participacion foreign key (id_participacion)
		references tbl_participacion (id_participacion)
		on update cascade on delete restrict,
	constraint fk_concepto foreign key (concepto)
		references tbl_conceptos (concepto)
		on update cascade on delete restrict,
	constraint fk_nombre_coach foreign key (nombre_coach)
		references tbl_coaches (nombre_coach)
		on update cascade on delete restrict,
	constraint fk_id_actividad foreign key (id_actividad)
		references tbl_actividades (id_actividad)
		on update cascade on delete restrict,
	constraint uq_participacion_actividad unique (id_participacion, id_actividad),
	constraint concepto_actividades check (concepto = 'Actividades')
) inherits (tbl_registros_);

alter table tbl_registros_actividades alter column concepto set default 'Actividades';

create index if not exists tbl_reg_act_fki_concepto
	on tbl_registros_actividades using btree (concepto asc nulls last);

create index if not exists tbl_reg_act_fki_id_participacion
	on tbl_registros_actividades using btree (id_participacion asc nulls last);

create index if not exists tbl_reg_act_fki_nombre_coach
	on tbl_registros_actividades using btree (nombre_coach asc nulls last);

create index if not exists tbl_reg_act_fki_id_actividad
	on tbl_registros_actividades using btree (id_actividad asc nulls last);

-- Crear fn check_reto: No se puede registrar asistencia a actividaes que no corresponden al mismo reto
create or replace function check_reto()
	returns trigger as
	$BODY$
	declare
		reto_act tbl_retos.nombre_reto%type;
		reto_part tbl_retos.nombre_reto%type;
	begin
		select into reto_act nombre_reto
		from tbl_actividades ta
		where ta.id_actividad = new.id_actividad;
		
		select into reto_part nombre_reto
		from tbl_participacion tp
		where tp.id_participacion = new.id_participacion;
		
		if not (reto_act = reto_part)
		then
			raise exception 'Actividad % no corresponde a %',
			new.id_actividad,
			reto_part;
		end if;
		return new;
	end;
	$BODY$
	language plpgsql volatile;

drop trigger if exists trig_01_tbl_reg_act_check_reto on tbl_registros_actividades;
create trigger trig_01_tbl_reg_act_check_reto
	before insert or update of id_actividad, id_participacion on tbl_registros_actividades
	for each row execute function
	check_reto();

drop trigger if exists trig_02_tbl_reg_act_check_participacion_activa on tbl_registros_actividades;
create trigger trig_02_tbl_reg_act_check_participacion_activa
	before insert or delete or update of id_participacion, id_actividad on tbl_registros_actividades
	for each row execute function
	check_participacion_activa();

-- Crear tbl_habitos
create table if not exists tbl_habitos (
	id_habito int generated by default as identity primary key,
	nombre_habito varchar not null unique,
	peso_habito numeric (3,2) not null,
	constraint peso_valido check (peso_habito between 0.0 and 1.0)
);

-- Crear fn check_sum_habitos: La suma de pesos debe ser igual a 1
create or replace function check_sum_habitos()
	returns trigger as
	$BODY$
	declare
		sum_pesos tbl_habitos.peso_habito%type;
	begin
		select into sum_pesos sum (peso_habito)
		from tbl_habitos;
		
		if sum_pesos > 1.0
		then
			raise exception 'Definir el peso de % como % excede la suma total por %',
			new.nombre_habito,
			new.peso_habito,
			sum_pesos - 1;
		end if;
		return new;
	end;
	$BODY$
	language plpgsql volatile;

create trigger trig_01_tbl_habitos_check_sum_habitos
	after insert or update on tbl_habitos
	for each row execute function
	check_sum_habitos();

insert into tbl_habitos (nombre_habito, peso_habito)
	values
		('Desayuno', 0.12),
		('Media Mañana', 0.12),
		('Almuerzo', 0.12),
		('Media Tarde', 0.12),
		('Cena', 0.12),
		('Hidratación', 0.2),
		('Actividad Física', 0.2);

-- Crear tbl_registros_habitos
create table if not exists tbl_registros_habitos (
	id_registro_habito int generated by default as identity primary key,
	fecha_ocurrencia date not null,
	nombre_habito varchar not null,
	constraint fk_id_participacion foreign key (id_participacion)
		references tbl_participacion (id_participacion)
		on update cascade on delete restrict,
	constraint fk_concepto foreign key (concepto)
		references tbl_conceptos (concepto)
		on update cascade on delete restrict,
	constraint fk_nombre_habito foreign key (nombre_habito)
		references tbl_habitos (nombre_habito)
		on update cascade on delete restrict,
	constraint fk_nombre_coach foreign key (nombre_coach)
		references tbl_coaches (nombre_coach)
		on update cascade on delete restrict,
	constraint uq_participacion_fecha_habito unique (id_participacion, fecha_ocurrencia, nombre_habito),
	constraint concepto_habitos check (concepto = 'Habitos')
) inherits (tbl_registros_);

alter table tbl_registros_habitos alter column concepto set default 'Habitos';

create index if not exists tbl_reg_hab_fki_concepto
	on tbl_registros_habitos using btree (concepto asc nulls last);

create index if not exists tbl_reg_hab_fki_id_participacion
	on tbl_registros_habitos using btree (id_participacion asc nulls last);

create index if not exists tbl_reg_hab_fki_nombre_habito
	on tbl_registros_habitos using btree (nombre_habito asc nulls last);

create index if not exists tbl_reg_hab_fki_nombre_coach
	on tbl_registros_habitos using btree (nombre_coach asc nulls last);

-- Crear fn check_date_range_habito
create or replace function check_date_range_habito()
	returns trigger as
	$BODY$
	declare
		reto tbl_retos.nombre_reto%type;
		duracion tbl_retos.duracion_reto%type ;
	begin
		select into reto nombre_reto
		from tbl_participacion tp
		where tp.id_participacion = new.id_participacion;
		
		select into duracion duracion_reto
		from tbl_retos tr
		where tr.nombre_reto = reto;
		
		if not (new.fecha_ocurrencia <@ duracion) then 
			raise exception 'Fecha % fuera de rango %',
			new.fecha_ocurrencia,
			duracion;
		end if;
		
		if new.fecha_ocurrencia > current_date then 
			raise exception 'Fecha % mayor que la fecha actual %',
			new.fecha_ocurrencia,
			current_date;
		end if;
		return new;
	end;
	$BODY$
	language plpgsql volatile;

drop trigger if exists trig_01_tbl_reg_hab_check_participacion_activa on tbl_registros_habitos;
create trigger trig_01_tbl_reg_hab_check_participacion_activa
	before insert or delete or update of id_participacion, fecha_ocurrencia, nombre_habito on tbl_registros_habitos
	for each row execute function
	check_participacion_activa();

drop trigger if exists trig_02_tbl_reg_hab_check_date_range_habito on tbl_registros_habitos;
create trigger trig_02_tbl_reg_hab_check_date_range_habito
	before insert or update of fecha_ocurrencia on tbl_registros_habitos
	for each row execute function
	check_date_range_habito();

drop trigger if exists trig_03_tbl_reg_hab_update_register_date on tbl_registros_habitos;
create trigger trig_03_tbl_reg_hab_update_register_date
	before insert or update on tbl_registros_habitos
	for each row execute function
	update_register_date();

-- Crear tbl_criterios_calificacion
create table if not exists tbl_criterios_calificacion (
	id_criterio_calificacion int generated by default as identity primary key,
	criterio_calificacion varchar not null unique,
	peso_criterio numeric (3,2) not null,
	constraint peso_valido check (peso_criterio between 0.0 and 1.0)	
);

-- Crear fn check_sum_criterios: La suma de pesos debe ser igual a 1
create or replace function check_sum_criterios()
	returns trigger as
	$BODY$
	declare
		sum_pesos tbl_criterios_calificacion.peso_criterio%type;
	begin
		select into sum_pesos sum (peso_criterio)
		from tbl_criterios_calificacion;
		
		if sum_pesos > 1.0
		then
			raise exception 'Definir el peso de % como % excede la suma total por %',
			new.criterio_calificacion,
			new.peso_criterio,
			sum_pesos - 1;
		end if;
		return new;
	end;
	$BODY$
	language plpgsql volatile;

create trigger trig_01_tbl_creiterios_calificacion_check_sum_criterios
	after insert or update on tbl_criterios_calificacion
	for each row execute function
	check_sum_criterios();

insert into tbl_criterios_calificacion (criterio_calificacion, peso_criterio)
	values
		('Variación de Peso', 0.3),
		('Variación de Medidas', 0.3),
		('Apariencia Física', 0.4);

-- Crear tbl_registros_calificaciones
create table if not exists tbl_registros_calificaciones (
	id_registro_calificacion int generated by default as identity primary key,
	criterio_calificacion varchar not null,
	calificacion int not null,
	constraint calificacion_valida check (calificacion <@ '[-3, 3]'::int4range),
	constraint fk_id_participacion foreign key (id_participacion)
		references tbl_participacion (id_participacion)
		on update cascade on delete restrict,
	constraint fk_concepto foreign key (concepto)
		references tbl_conceptos (concepto)
		on update cascade on delete restrict,
	constraint fk_criterio_calificacion foreign key (criterio_calificacion)
		references tbl_criterios_calificacion (criterio_calificacion)
		on update cascade on delete restrict,
	constraint fk_nombre_coach foreign key (nombre_coach)
		references tbl_coaches (nombre_coach)
		on update cascade on delete restrict,
	constraint uq_participacion_criterio_coach unique (id_participacion, criterio_calificacion, nombre_coach),
	constraint concepto_calificacion check (concepto = 'Calificacion')
) inherits (tbl_registros_);

alter table tbl_registros_calificaciones alter column concepto set default 'Calificacion';

create index if not exists tbl_reg_cal_fki_concepto
	on tbl_registros_calificaciones using btree (concepto asc nulls last);

create index if not exists tbl_reg_cal_fki_id_participacion
	on tbl_registros_calificaciones using btree (id_participacion asc nulls last);

create index if not exists tbl_reg_cal_fki_criterio_calificacion
	on tbl_registros_calificaciones using btree (criterio_calificacion asc nulls last);

create index if not exists tbl_reg_cal_fki_nombre_coach
	on tbl_registros_calificaciones using btree (nombre_coach asc nulls last);

drop trigger if exists trig_01_tbl_reg_cal_check_participacion_activa on tbl_registros_calificaciones;
create trigger trig_01_tbl_reg_cal_check_participacion_activa
	before insert or delete or update of id_participacion, criterio_calificacion, calificacion on tbl_registros_calificaciones
	for each row execute function
	check_participacion_activa();

drop trigger if exists trig_02_tbl_reg_cal_update_register_date on tbl_registros_calificaciones;
create trigger trig_02_tbl_reg_cal_update_register_date
	before insert or update on tbl_registros_calificaciones
	for each row execute function
	update_register_date();