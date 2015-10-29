Обновление: Загружена альфа версия VBOMesh 2.0

Из основных изменений:<br>
1. Отказ от системы текстур и материалов сцены (использование только через класс-адаптер)<br>
2. Отказ от сценовских загрузчиков геометрии 3ds/obj;<br>
3. Добавлены новые объекты - коллекция, контейнер, эффект, скрипт;<br>
4. Изменение в архитектуре модуля:<br>
<ul>
<li> Создан базовый класс TVBOMeshItem, от которого наследуются TMovableObject, TMeshCollection, TMeshContainer;</li>
<li> TVBOMesh теперь наследуется от TMeshCollection и может хранить указатели на TMeshContainer, TMeshCollection, TVBOMeshObject и другие, наследованные от TVBOMeshItem;</li>
<li> Создан класс TMovableObject(TVBOMeshItem), в который перенесены все методы TVBOMeshObject по управлению трансформациями объекта;</li>
<li> TVBOMeshObject наследуется от TMovableObject;</li>
<li> Базовые классы TVBOMeshItem и TMovableObject, а так же некоторые базовые типы перенесены в модуль uBaseClasses;</li>
<li> Все базовые примитивы, включая фриформы и скелетную анимацию, перенесены в модуль uMeshObjects.</li>
</ul>
Ну и тысячи сопутствующих изменений во всех модулях.<br>


<p>Цель проекта - расширение существующего функционала <a href='http://glscene.org'>GLScene</a> за счет перехода на VBO и внедрение в проект таких технологий для работы с буферами как <a href='http://en.wikipedia.org/wiki/Framebuffer_Object'>FBO</a>, <a href='http://www.gamedev.ru/code/terms/MRT'>MRT</a>, <a href='http://songho.ca/opengl/gl_pbo.html'>PBO</a> и <a href='http://www.devmaster.net/forums/showthread.php?t=7457'>R2VB</a>. Автор проекта AKarpenyuk (aka Fantom) </p>

Основные возможности:<br>
<ul>
<li>  Вывод графических примитивов и объектов в формате obj/3ds/smd посредством <a href='http://www.gamedev.ru/code/terms/VBO'>VBO</a>; </li>
<li>  Проигрывание анимации из 3ds по ключевым кадрам </li>
<li>  Менеджер частиц на основе <a href='http://oss.sgi.com/projects/ogl-sample/registry/ARB/point_sprite.txt'>PointSprite</a>;</li>
<li>  Анимированные спрайты;</li>
<li>  Интегрированный рендеринг в текстуру посредством <a href='http://en.wikipedia.org/wiki/Framebuffer_Object'>FBO</a>;</li>
<li>  Менеджер шейдерных объектов (<a href='http://ru.wikipedia.org/wiki/OpenGL_Shading_Language'>GLSL</a>);</li>
<li>  Скелетная анимация на GPU посредством технологии <a href='http://www.devmaster.net/forums/showthread.php?t=7457'>R2VB</a>;</li>
<li>  Скелетная анимация на GPU с передачей костей через юниформы;</li>
<li>  Система LOD'ов для геометрии;</li>
<li>  Работа с текстурами (чтение/запись посредством <a href='http://songho.ca/opengl/gl_pbo.html'>PBO</a>/<a href='http://tech-artists.org/wiki/Fixed-Function_Pipeline'>FFP</a>), материалами и источниками света;</li>
<li>  Управление камерой от третьего лица;</li>
<li>  Ландшафт на VBO;</li>
<li>  Перекрытие функций OGL для фонового кеширования стэйтов;</li>
<li>  Рисование объемных линий;</li>
<li>  Группировка объектов по библиотекам;</li>
<li>  Проверка видимости через <a href='http://www.opengl.org/registry/specs/ARB/occlusion_query.txt'>Occlusion Culling</a>;</li>
</ul>

В разработке:<br>
<ul>
<li> GUI на VBO;</li>
<li> Шейдерные материалы;</li>
<li> Библиотека шейдеров;</li>
<li> Скриптовый рендер;</li>
<li> Оптимизация сцены за счет создания единого буфера статических объектов;</li>
<li> Создание VCL компонентов и отказ от рендера сцены.</li>
</ul>

Обсуждение движка тут: <a href='http://www.glscene.ru/forum_viewforum.php?29'>http://www.glscene.ru/forum_viewforum.php?29</a> <br>
Блог, посвященный движку тут: <a href='http://vbomesh.blogspot.com'>http://vbomesh.blogspot.com</a><br>