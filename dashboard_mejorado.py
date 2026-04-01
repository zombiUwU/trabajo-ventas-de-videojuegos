# para instalar paquetes
# pip install pandas numpy streamlit matplotlib plotly seaborn os plotly.express

### Importamos la paquetería ###
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import streamlit as st
import seaborn as sns
import plotly.express as px
import os

# --- CONFIGURACIÓN DE LA PÁGINA ---
st.set_page_config(
    page_title="Dashboard de Ventas de Videojuegos",
    page_icon="🎮️",
    layout="wide",
    initial_sidebar_state="expanded",
)

# --- TÍTULOS Y DESCRIPCIÓN MEJORADOS ---
st.title("🎮 Dashboard de Ventas de Videojuegos 🎮")
st.markdown("---") # Separador visual
st.markdown("Explora las ventas globales de videojuegos, analizando las regiones de: Japón, Estados unidos, Europa y otros, analizamos las ventas y su distribución en los generos y la plataforma de lanzamiento.")

# Nombre del archivo y DataFrame (usando ruta relativa para compatibilidad cross-platform)
FILE_PATH = os.path.join(os.getcwd(), "video_games_sales_corregido.csv")
DATA_NAME = "data"

# --- Carga de Datos y Caché ---
@st.cache_data
def load_and_prepare_data(path):
    # Cargar y asignar al nombre 'data'
    data = pd.read_csv(path)
    
    # Limpieza básica: rellenar nulos en 'Year' y convertir a entero
    data['Year'] = data['Year'].fillna(-1).astype(int)
    data = data[data['Year'] > 0].copy() 
    
    # CORRECCIÓN DE TIPO: Tratar los valores nulos en 'Publisher' y convertirlos a string
    data['Publisher'] = data['Publisher'].fillna('Unknown Publisher').astype(str)
    
    # Crear una columna de ventas total para el ranking de Publisher
    data['Total_Sales_Publisher'] = data.groupby('Publisher')['Global_Sales'].transform('sum')
    
    return data

try:
    data = load_and_prepare_data(FILE_PATH)
except FileNotFoundError:
    st.error(f"Error: No se encontró el archivo {FILE_PATH}. Asegúrate de que el archivo esté en el mismo directorio que este script.")
    st.stop()
except Exception as e:
    st.error(f"Ocurrió un error al cargar o procesar el archivo: {e}")
    st.stop()

# --- BARRA LATERAL (Filtros Interactivos con Iconos) ---
st.sidebar.header("⚙️ Opciones de Filtrado")

# Filtro por Rango de Año
min_year = int(data['Year'].min())
max_year = int(data['Year'].max())
rango_anio = st.sidebar.slider(
    "📅 **Rango de Año:**", 
    min_value=min_year,
    max_value=max_year,
    value=(min_year, max_year)
)


# Filtro por Género
generos_disponibles = sorted(data['Genre'].unique().tolist())
generos_seleccionados = st.sidebar.multiselect(
    "🕹️ **Géneros:**", 
    options=generos_disponibles,
    default=generos_disponibles[:5] 
)

# Filtro por Plataforma
plataformas_disponibles = sorted(data['Platform'].unique().tolist())
plataformas_seleccionadas = st.sidebar.multiselect(
    "🖥️ **Plataformas:**", 
    options=plataformas_disponibles,
    default=plataformas_disponibles[:5] 
)

# Filtro por Publisher
publishers_disponibles = sorted(data['Publisher'].unique().tolist())
publishers_seleccionados = st.sidebar.multiselect(
    "🏢 **Publishers:**", 
    options=publishers_disponibles,
    default=[p for p in publishers_disponibles if p != 'Unknown Publisher'][:30]
)

# Aplicar filtros
data_filtrada = data[
    (data['Genre'].isin(generos_seleccionados)) &
    (data['Platform'].isin(plataformas_seleccionadas)) &
    (data['Year'] >= rango_anio[0]) &
    (data['Year'] <= rango_anio[1]) &
    (data['Publisher'].isin(publishers_seleccionados))
].copy()

# Mensaje si no hay datos
if data_filtrada.empty:
    st.warning("⚠️ No hay datos que coincidan con los filtros seleccionados. Ajusta tus selecciones.")
    st.stop()

# --- CREAR PESTAÑAS (Con Iconos en Markdown) ---
tab1, tab2, tab3, tab4, tab5 = st.tabs(
    [
        "**💰 Métricas Clave**", 
        "**📈 Ventas y Género**", 
        "**🌎 Distribución Regional**", 
        "**🔗 Correlación**", 
        "**👑 Top Publishers**"
    ]
)

# --- CONFIGURACIÓN DE ESTILO PARA MATPLOTLIB/SEABORN (Fondo Oscuro) ---
plt.style.use('dark_background') 

with tab1:
    # --- MÉTRICAS CLAVE (KPIs con Estilo MEJORADO) ---
    st.header("Indicadores Clave de Rendimiento (KPIs) 🎯")
    
    # Usamos un st.container para darles una sensación de bloque
    with st.container(border=True): 
        total_ventas = data_filtrada['Global_Sales'].sum()
        num_juegos = data_filtrada.shape[0]
        
        col_kpi1, col_kpi2, col_kpi3 = st.columns(3)
        
        with col_kpi1:
            st.markdown("##### **💸 Ventas Globales Totales**")
            st.metric(label="Millones USD", value=f"{total_ventas:,.2f} M", delta_color="off")
        
        with col_kpi2:
            st.markdown("##### **🔢 Número de Títulos**")
            st.metric(label="Juegos", value=f"{num_juegos:,}", delta_color="off")
        
        with col_kpi3:
            st.markdown("##### **⭐ Venta Promedio por Título**")
            if num_juegos > 0:
                st.metric(label="Millones USD", value=f"{(total_ventas / num_juegos):.2f} M", delta_color="off")
            else:
                st.metric(label="Millones USD", value="N/A", delta_color="off")

    st.info(f"**Filtros aplicados:** Datos desde el año **{rango_anio[0]}** al **{rango_anio[1]}**, incluyendo {len(generos_seleccionados)} géneros y {len(plataformas_seleccionadas)} plataformas.")


with tab2:
    # --- GRÁFICOS INTERACTIVOS (Plotly Express - Tema Oscuro) ---
    st.header("Análisis de Ventas por Género y Año 📈")
    col_px1, col_px2 = st.columns(2)
    
    # Gráfico 1: Ventas por Género 
    with col_px1:
        st.subheader("Top 10 Géneros por Ventas Globales 🥇")
        
        ventas_por_genero = data_filtrada.groupby('Genre')['Global_Sales'].sum().reset_index().rename(columns={'Global_Sales': 'Ventas'})
        
        fig_px_bar = px.bar(
            ventas_por_genero.sort_values(by='Ventas', ascending=False).head(10),
            x='Genre',
            y='Ventas',
            title='Ventas Globales por Género (M $)',
            labels={'Genre': 'Género', 'Ventas': 'Ventas Globales (M $)'},
            color='Genre',
            template='plotly_dark'
        )
        # MEJORA: Hover template más limpio
        fig_px_bar.update_traces(hovertemplate='Género: %{x}<br>Ventas: %{y:.2f} M$<extra></extra>') 
        fig_px_bar.update_layout(xaxis={'categoryorder':'total descending'}, title_x=0.5)
        st.plotly_chart(fig_px_bar, use_container_width=True)
    
    # Gráfico 2: Evolución Anual de Ventas Globales 
    with col_px2:
        st.subheader("Evolución Anual de Ventas Globales 📅")
        
        ventas_por_anio = data_filtrada.groupby('Year')['Global_Sales'].sum().reset_index().rename(columns={'Global_Sales': 'Ventas'})
        
        fig_px_line = px.line(
            ventas_por_anio,
            x='Year',
            y='Ventas',
            title='Tendencia de Ventas Globales por Año',
            labels={'Year': 'Año', 'Ventas': 'Ventas Globales (M $)'},
            markers=True,
            template='plotly_dark'
        )
        # MEJORA: Hover template más limpio
        fig_px_line.update_traces(hovertemplate='Año: %{x}<br>Ventas: %{y:.2f} M$<extra></extra>') 
        fig_px_line.update_xaxes(type='category')
        fig_px_line.update_layout(title_x=0.5)
        st.plotly_chart(fig_px_line, use_container_width=True)

with tab3:
    # --- GRÁFICO ACTUALIZADO: Distribución Regional (Gráfico de Barras Agrupadas Plotly) ---
    st.header("Distribución Regional de Ventas 🌍")
    st.subheader("Comparación de Ventas Regionales por Plataforma (M $)")
    
    # Agrupar las ventas regionales y resetear el índice para Plotly
    ventas_regionales_platform = data_filtrada.groupby('Platform')[['NA_Sales', 'EU_Sales', 'JP_Sales', 'Other_Sales']].sum().reset_index()
    
    # Derretir el DataFrame (unpivot) para Plotly (Platform, Region, Sales)
    df_melted = ventas_regionales_platform.melt(
        id_vars='Platform',
        value_vars=['NA_Sales', 'EU_Sales', 'JP_Sales', 'Other_Sales'],
        var_name='Region',
        value_name='Sales_M'
    )

    # Crear el Gráfico de Barras Agrupadas
    fig_px_dist = px.bar(
        df_melted,
        x='Platform',
        y='Sales_M',
        color='Region',
        barmode='group', # Agrupar las barras en lugar de apilarlas
        title='Ventas Regionales por Plataforma (M $)',
        labels={'Platform': 'Plataforma', 'Sales_M': 'Ventas (M $)', 'Region': 'Región'},
        template='plotly_dark'
    )
    fig_px_dist.update_layout(title_x=0.5)
    st.plotly_chart(fig_px_dist, use_container_width=True)


with tab4:
    # --- GRÁFICO ACTUALIZADO: Correlación (Gráfico de Dispersión Lineal Plotly) ---
    st.header("Análisis de Correlación entre Regiones 🔗")
    
    st.subheader("Relación entre Ventas en Norteamérica y Europa")
    
    # Creamos un filtro para la correlación específica (solo para el gráfico de dispersión)
    col_x, col_y = st.columns(2)
    
    sales_cols_corr = ['NA_Sales', 'EU_Sales', 'JP_Sales', 'Other_Sales']
    
    corr_x = col_x.selectbox('Eje X (Ventas de la Región):', options=sales_cols_corr, index=0) # NA_Sales por defecto
    corr_y = col_y.selectbox('Eje Y (Ventas de la Región):', options=sales_cols_corr, index=1) # EU_Sales por defecto
    
    # Crear el gráfico de dispersión interactivo con línea de tendencia
    fig_px_scatter = px.scatter(
        data_filtrada,
        x=corr_x,
        y=corr_y,
        # Usamos 'Genre' o 'Platform' como color para diferenciar los puntos (más informativo)
        color='Genre', 
        opacity=0.6,
        trendline="ols", # Línea de tendencia de mínimos cuadrados ordinarios
        title=f'Correlación entre {corr_x} y {corr_y}',
        labels={corr_x: f'{corr_x} (M $)', corr_y: f'{corr_y} (M $)'},
        template='plotly_dark',
        hover_data=['Name', 'Platform'] # Mostrar nombre del juego al pasar el ratón
    )
    
    fig_px_scatter.update_layout(title_x=0.5)
    st.plotly_chart(fig_px_scatter, use_container_width=True)
    
    # Opcional: Mostrar el coeficiente de correlación simple
    corr_value = data_filtrada[[corr_x, corr_y]].corr().iloc[0, 1]
    st.info(f"**Coeficiente de Correlación ({corr_x} vs {corr_y}):** `{corr_value:.3f}`")


with tab5:
    st.header("👑 Top Publishers por Ventas Globales")
    
    # --- MEJORA: Slider de Ranking ---
    top_n = st.slider(
        "Selecciona el número de Publishers a mostrar (N):",
        min_value=5,
        max_value=30,
        value=15,
        step=5
    )

    # Agrupación y cálculo de ventas por Publisher
    ventas_por_publisher = data_filtrada.groupby('Publisher')['Global_Sales'].sum().reset_index().rename(columns={'Global_Sales': 'Ventas'})
    
    # Obtener el Top N para la visualización
    top_publishers = ventas_por_publisher.sort_values(by='Ventas', ascending=False).head(top_n)
    
    st.markdown(f"Mostrando **Top {top_n} Publishers** en el rango de año *{rango_anio[0]} - {rango_anio[1]}* y filtros aplicados. ")
    
    # Gráfico de Barras Interactivo (Plotly Express)
    fig_px_publisher = px.bar(
        top_publishers,
        x='Publisher',
        y='Ventas',
        title=f'Top {top_n} Publishers por Ventas Globales (M $)',
        labels={'Publisher': '🏢 Publisher', 'Ventas': 'Ventas Globales (M $)'},
        color='Publisher',
        template='plotly_dark' 
    )
    # MEJORA: Hover template más limpio
    fig_px_publisher.update_traces(hovertemplate='Publisher: %{x}<br>Ventas: %{y:.2f} M$<extra></extra>')
    fig_px_publisher.update_layout(xaxis={'categoryorder':'total descending'}, title_x=0.5)
    st.plotly_chart(fig_px_publisher, use_container_width=True)


# --- TABLA DE DATOS (En Expander) ---
st.markdown("---")
with st.expander("🔍 **Ver Datos Crudos Filtrados**", expanded=False): # Por defecto, cerrado
    st.dataframe(
        data_filtrada[['Name', 'Platform', 'Year', 'Genre', 'Publisher', 'Global_Sales']].sort_values(
            by='Global_Sales', 
            ascending=False
        ), 
        use_container_width=True
    )
st.markdown("---")