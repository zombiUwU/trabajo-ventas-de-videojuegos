
# para instalar paquetes
# pip install pandas numpy streamlit matplotlib plotly seaborn

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
    initial_sidebar_state="expanded"
)

st.title("🚀 Dashboard Interactivo de Ventas de Videojuegos ")
st.markdown("Explora las ventas globales de videojuegos con visualizaciones dinámicas.")

# Nombre del archivo y DataFrame (usando ruta relativa para compatibilidad cross-platform)
FILE_PATH = os.path.join(os.getcwd(), "video_games_sales_corregido.csv")  # Ruta relativa al directorio actual
DATA_NAME = "data"

# --- Carga de Datos y Caché ---
@st.cache_data
def load_and_prepare_data(path):
    # Cargar y asignar al nombre 'data'
    data = pd.read_csv(path)
    
    # Limpieza básica: rellenar nulos en 'Year' y convertir a entero
    # Rellenamos con un valor temporal (-1) antes de la conversión para manejar nulos de forma segura.
    data['Year'] = data['Year'].fillna(-1).astype(int)
    data = data[data['Year'] > 0].copy()  # Eliminar filas sin año válido
    
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

# --- BARRA LATERAL (Filtros Interactivos) ---
st.sidebar.header("Opciones de Filtrado")

# Filtro por Género
generos_disponibles = sorted(data['Genre'].unique().tolist())
generos_seleccionados = st.sidebar.multiselect(
    "Selecciona Géneros:",
    options=generos_disponibles,
    default=generos_disponibles[:5]  # Increased default selection
)

# Filtro por Plataforma
plataformas_disponibles = sorted(data['Platform'].unique().tolist())
plataformas_seleccionadas = st.sidebar.multiselect(
    "Selecciona Plataformas:",
    options=plataformas_disponibles,
    default=plataformas_disponibles[:5]  # Increased default selection
)

# Filtro por Rango de Año
min_year = int(data['Year'].min())
max_year = int(data['Year'].max())
rango_anio = st.sidebar.slider(
    "Selecciona Rango de Año:",
    min_value=min_year,
    max_value=max_year,
    value=(min_year, max_year)
)

# Aplicar filtros
data_filtrada = data[
    (data['Genre'].isin(generos_seleccionados)) &
    (data['Platform'].isin(plataformas_seleccionadas)) &
    (data['Year'] >= rango_anio[0]) &
    (data['Year'] <= rango_anio[1])
].copy()

# Mensaje si no hay datos
if data_filtrada.empty:
    st.warning("⚠️ No hay datos que coincidan con los filtros seleccionados.")
    st.stop()

# --- CREAR PESTAÑAS ---
tab1, tab2, tab3, tab4 = st.tabs(["Métricas Clave", "Ventas por Género y Año", "Distribución Regional", "Correlación de Ventas"])

with tab1:
    # --- MÉTRICAS CLAVE (KPIs) ---
    st.header("Indicadores Clave de Rendimiento (KPIs)")
    total_ventas = data_filtrada['Global_Sales'].sum()
    num_juegos = data_filtrada.shape[0]
    
    col_kpi1, col_kpi2, col_kpi3 = st.columns(3)
    
    with col_kpi1:
        st.metric(label="Ventas Globales Totales (M $)", value=f"{total_ventas:,.2f} M")
    
    with col_kpi2:
        st.metric(label="Número de Juegos", value=f"{num_juegos:,}")
    
    with col_kpi3:
        if num_juegos > 0:
            st.metric(label="Venta Promedio por Juego (M $)", value=f"{(total_ventas / num_juegos):.2f} M")
        else:
            st.metric(label="Venta Promedio por Juego (M $)", value="N/A")

with tab2:
    # --- GRÁFICOS INTERACTIVOS (Plotly Express) ---
    st.header("Análisis de Ventas por Género y Año")
    col_px1, col_px2 = st.columns(2)
    
    # Gráfico 1: Ventas por Género (Plotly Express - Barras Interactivas)
    with col_px1:
        st.subheader("Ventas Globales por Género")
        
        ventas_por_genero = data_filtrada.groupby('Genre')['Global_Sales'].sum().reset_index().rename(columns={'Global_Sales': 'Ventas'})
        
        fig_px_bar = px.bar(
            ventas_por_genero.sort_values(by='Ventas', ascending=False).head(10),
            x='Genre',
            y='Ventas',
            title='Top 10 Géneros por Ventas Globales',
            labels={'Genre': 'Género', 'Ventas': 'Ventas Globales (M $)'},
            color='Genre',
            template='plotly_white'  # Improved template
        )
        # Mejorar el layout para Plotly
        fig_px_bar.update_layout(xaxis={'categoryorder':'total descending'})
        st.plotly_chart(fig_px_bar, use_container_width=True)
    
    # Gráfico 2: Evolución Anual de Ventas Globales (Plotly Express - Líneas Interactivas)
    with col_px2:
        st.subheader("Evolución Anual de Ventas Globales")
        
        ventas_por_anio = data_filtrada.groupby('Year')['Global_Sales'].sum().reset_index().rename(columns={'Global_Sales': 'Ventas'})
        
        fig_px_line = px.line(
            ventas_por_anio,
            x='Year',
            y='Ventas',
            title='Tendencia de Ventas Globales por Año',
            labels={'Year': 'Año', 'Ventas': 'Ventas Globales (M $)'},
            markers=True,
            template='plotly_white'  # Improved template
        )
        fig_px_line.update_xaxes(type='category')  # Treat years as categories for better display
        st.plotly_chart(fig_px_line, use_container_width=True)

with tab3:
    # --- GRÁFICO ADICIONAL (Seaborn/Matplotlib) ---
    st.header("Distribución Regional de Ventas")
    st.subheader("Proporción de Ventas Regionales por Plataforma")
    
    # Agrupar las ventas regionales
    ventas_regionales = data_filtrada.groupby('Platform')[['NA_Sales', 'EU_Sales', 'JP_Sales', 'Other_Sales']].sum()
    # Normalizar los datos para el gráfico de calor
    ventas_normalizadas = ventas_regionales.div(ventas_regionales.sum(axis=1), axis=0).fillna(0)
    
    # Crear la figura de Matplotlib para Seaborn
    fig_sns, ax_sns = plt.subplots(figsize=(12, max(6, len(ventas_normalizadas.index) * 0.5)))  # Dynamic height
    sns.heatmap(
        ventas_normalizadas.T * 100,  # Transponer y multiplicar por 100 para porcentaje
        cmap="YlGnBu",
        annot=True,
        fmt=".1f",  # Formato de un decimal
        cbar_kws={'label': 'Proporción de Ventas (%)'},
        linewidths=.5,  # Added linewidths
        ax=ax_sns
    )
    
    ax_sns.set_title('Proporción de Ventas Regionales por Plataforma', fontsize=16)
    ax_sns.set_ylabel('Región', fontsize=12)
    ax_sns.set_xlabel('Plataforma', fontsize=12)
    plt.yticks(rotation=0)  # Ensure y-axis labels are horizontal
    plt.tight_layout()
    
    # Mostrar la figura de Matplotlib/Seaborn en Streamlit
    st.pyplot(fig_sns)

with tab4:
    # --- GRÁFICO DE CORRELACIÓN (Seaborn) ---
    st.header("Análisis de Correlación")
    st.subheader("Matriz de Correlación de Ventas Regionales")
    
    sales_cols = ['NA_Sales', 'EU_Sales', 'JP_Sales', 'Other_Sales', 'Global_Sales']
    correlation_matrix = data_filtrada[sales_cols].corr()
    
    # Crear la figura de Matplotlib para el heatmap de correlación
    fig_corr, ax_corr = plt.subplots(figsize=(8, 6))
    sns.heatmap(
        correlation_matrix,
        annot=True,
        cmap="coolwarm",
        fmt=".2f",
        linewidths=.5,
        ax=ax_corr
    )
    
    ax_corr.set_title('Matriz de Correlación de Ventas Regionales', fontsize=16)
    plt.tight_layout()
    st.pyplot(fig_corr)

# --- TABLA DE DATOS ---
st.markdown("---")
st.subheader("Tabla de Datos Filtrados")
# Mostramos un subconjunto de columnas para mayor claridad
st.dataframe(data_filtrada[['Name', 'Platform', 'Year', 'Genre', 'Publisher', 'Global_Sales']].sort_values(by='Global_Sales', ascending=False), use_container_width=True)

