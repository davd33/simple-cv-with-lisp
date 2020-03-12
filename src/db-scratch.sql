drop table contact;
drop table cv;
drop table work_experience;
drop table reading;
drop table paragraph_element;

select * from cv cv
join contact co on co.id=cv.contact_id
full outer join work_experience we on we.cv_id=cv.id
full outer join reading r on r.cv_id=cv.id
full outer join paragraph_element pe on pe.cv_id=cv.id
where cv.title='newline31';
